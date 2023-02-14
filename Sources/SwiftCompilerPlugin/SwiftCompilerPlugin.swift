//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftSyntaxMacros
import SwiftSyntax
import SwiftDiagnostics

@_implementationOnly import Foundation
#if os(Windows)
@_implementationOnly import ucrt
#endif

//
// This source file contains the main entry point for compiler plugins.
// A plugin receives messages from the "plugin host" (typically
// 'swift-frontend'), and sends back messages in return based on its actions.
//
// Exactly how the plugin host invokes a plugin is an implementation detail,
// but the current approach is to compile the Swift source files that make up
// the plugin into an executable for the host platform, and to then invoke the
// executable in a sandbox that blocks network access and prevents changes to
// all except for a few specific file system locations.
//
// The host process and the plugin communicate using messages in the form of
// length-prefixed JSON-encoded Swift enums. The host sends messages to the
// plugin through its standard-input pipe, and receives messages through the
// plugin's standard-output pipe. All output received through the plugin's
// standard-error pipe is considered to be free-form textual console output.
//
// Within the plugin process, `stdout` is redirected to `stderr` so that print
// statements from the plugin are treated as plain-text output, and `stdin` is
// closed so that any attemps by the plugin logic to read from console result
// in errors instead of blocking the process. The original `stdin` and `stdout`
// are duplicated for use as messaging pipes, and are not directly used by the
// plugin logic.
//
// The exit code of the plugin process indicates whether the plugin invocation
// is considered successful. A failure result should also be accompanied by an
// emitted error diagnostic, so that errors are understandable by the user.
//
// Using standard input and output streams for messaging avoids having to make
// allowances in the sandbox for other channels of communication, and seems a
// more portable approach than many of the alternatives. This is all somewhat
// temporary in any case — in the long term, something like distributed actors
// or something similar can hopefully replace the custom messaging.
//
public protocol CompilerPlugin {
  init()

  var providingMarcos: [Any.Type] { get }
}

extension CompilerPlugin {

  /// Main entry point of the plugin — sets up a communication channel with
  /// the plugin host and runs the main message loop.
  public static func main() async throws {
    // Duplicate the `stdin` file descriptor, which we will then use for
    // receiving messages from the plugin host.
    let inputFD = dup(fileno(stdin))
    guard inputFD >= 0 else {
      internalError("Could not duplicate `stdin`: \(describe(errno: errno)).")
    }

    // Having duplicated the original standard-input descriptor, we close
    // `stdin` so that attempts by the plugin to read console input (which
    // are usually a mistake) return errors instead of blocking.
    guard close(fileno(stdin)) >= 0 else {
      internalError("Could not close `stdin`: \(describe(errno: errno)).")
    }

    // Duplicate the `stdout` file descriptor, which we will then use for
    // sending messages to the plugin host.
    let outputFD = dup(fileno(stdout))
    guard outputFD >= 0 else {
      internalError("Could not dup `stdout`: \(describe(errno: errno)).")
    }

    // Having duplicated the original standard-output descriptor, redirect
    // `stdout` to `stderr` so that all free-form text output goes there.
    guard dup2(fileno(stderr), fileno(stdout)) >= 0 else {
      internalError("Could not dup2 `stdout` to `stderr`: \(describe(errno: errno)).")
    }
    
    // Turn off full buffering so printed text appears as soon as possible.
    // Windows is much less forgiving than other platforms.  If line
    // buffering is enabled, we must provide a buffer and the size of the
    // buffer.  As a result, on Windows, we completely disable all
    // buffering, which means that partial writes are possible.
#if os(Windows)
    setvbuf(stdout, nil, _IONBF, 0)
#else
    setvbuf(stdout, nil, _IOLBF, 0)
#endif

    // Open a message channel for communicating with the plugin host.
    pluginHostConnection = PluginHostConnection(
      inputStream: FileHandle(fileDescriptor: inputFD),
      outputStream: FileHandle(fileDescriptor: outputFD))

    // Handle messages from the host until the input stream is closed,
    // indicating that we're done.
    let instance = Self()
    do {
      while let message = try pluginHostConnection.waitForNextMessage() {
        try await instance.handleMessage(message)
      }
    } catch {
      // Emit a diagnostic and indicate failure to the plugin host,
      // and exit with an error code.
      internalError(String(describing: error))
    }
  }

  // Private function to report internal errors and then exit.
  fileprivate static func internalError(_ message: String) -> Never {
    fputs("Internal Error: \(message)\n", stderr)
    exit(1)
  }

  // Private function to construct an error message from an `errno` code.
  fileprivate static func describe(errno: Int32) -> String {
    if let cStr = strerror(errno) { return String(cString: cStr) }
    return String(describing: errno)
  }

  /// Handles a single message received from the plugin host.
  fileprivate func handleMessage(_ message: HostToPluginMessage) async throws {
    switch message {
    case .getCapability:
      try pluginHostConnection.sendMessage(
        .getCapabilityResult(capability: PluginMessage.capability))
      break

    case .expandFreeStandingMacro(let macro, let expandingSyntax, let discriminator):
      try expandFreestandingMacro(
        macro: macro,
        expandingSyntax: expandingSyntax,
        discriminator: discriminator)

    case .expandAttachedMacro(let macro, let macroRole, let customAttributeSyntax, let declSyntax, let parentDeclSyntax):
      try expandAttachedMacro(
        macro: macro,
        macroRole: macroRole,
        customAttributeSyntax: customAttributeSyntax,
        declSyntax: declSyntax,
        parentDeclSyntax: parentDeclSyntax)

    default:
      Self.internalError("unexpected top-level message \(message)")
    }
  }

  private func expandFreestandingMacro(
    macro: PluginMessage.MacroReference,
    expandingSyntax: PluginMessage.Syntax,
    discriminator: String
  ) throws {
    let sourceManager = SourceManager()
    let context = PluginMacroExpansionContext(
      sourceManager: sourceManager, discriminator: discriminator)

    let syntax = sourceManager.add(expandingSyntax)
    guard let macroSyntax = syntax.asProtocol(FreestandingMacroExpansionSyntax.self) else {
      let diag = PluginMessage.Diagnostic(
        from: .init(node: syntax, message: MacroExpansionError.freestandingMacroSyntaxIsNotMacro),
        in: sourceManager)
      try pluginHostConnection.sendMessage(.expandFreeStandingMacroResult(
        expandedSource: "", diagnostics: [diag]))
      return
    }

    let macroDef = self.getMacroDefinition(macro)
    if let exprMacroDef = macroDef as? ExpressionMacro.Type {
      let rewritten = try exprMacroDef.expansion(of: macroSyntax, in: context)
      let diagnostics = context.diagnostics.map {
        PluginMessage.Diagnostic(from: $0, in: sourceManager)
      }

      let resultMessage = PluginToHostMessage.expandFreeStandingMacroResult(
        expandedSource: rewritten.description,
        diagnostics: diagnostics)

      try pluginHostConnection.sendMessage(resultMessage)
      return
    }

    if let declMacroDef = macroDef as? DeclarationMacro.Type {
      let rewritten = try declMacroDef.expansion(of: macroSyntax, in: context)
      let diagnostics = context.diagnostics.map {
        PluginMessage.Diagnostic(from: $0, in: sourceManager)
      }

      let resultMessage = PluginToHostMessage.expandFreeStandingMacroResult(
        expandedSource: rewritten.description,
        diagnostics: diagnostics)

      try pluginHostConnection.sendMessage(resultMessage)
      return
    }

    let diag = PluginMessage.Diagnostic(
      from: Diagnostic(node: syntax, message: MacroExpansionError.macroTypeNotFound),
      in: sourceManager)

    try pluginHostConnection.sendMessage(
      .expandFreeStandingMacroResult(expandedSource: "", diagnostics: [diag]))
  }

  private func expandAttachedMacro(
    macro: PluginMessage.MacroReference,
    macroRole: PluginMessage.MacroRole,
    customAttributeSyntax: PluginMessage.Syntax,
    declSyntax: PluginMessage.Syntax,
    parentDeclSyntax: PluginMessage.Syntax?
  ) throws {
    let sourceManager = SourceManager()
    let context = PluginMacroExpansionContext(
      sourceManager: sourceManager)

    guard let macroDefinition = getMacroDefinition(macro) else {
      fatalError("macro type not found: \(macro.moduleName).\(macro.typeName)")
    }

    let customAttributeNode = sourceManager.add(customAttributeSyntax).cast(AttributeSyntax.self)
    let declarationNode = sourceManager.add(declSyntax).cast(DeclSyntax.self)

    let expanded: String
    switch (macroDefinition, macroRole) {
    case (let attachedMacro as AccessorMacro.Type, .accessor):
      let accessors = try attachedMacro.expansion(
        of: customAttributeNode,
        providingAccessorsOf: declarationNode,
        in: context)
      expanded = accessors
        .map { $0.trimmedDescription }
        .joined(separator: "\n\n")

    case (let attachedMacro as MemberAttributeMacro.Type, .memberAttribute):
      guard
        let parentDeclSyntax = parentDeclSyntax,
        let parentDeclGroup = sourceManager.add(parentDeclSyntax).asProtocol(DeclGroupSyntax.self)
      else {
        fatalError("parentDecl is mandatory for MemberAttributeMacro")
      }

      // Local function to expand a member atribute macro once we've opened up
      // the existential.
      func expandMemberAttributeMacro<Node: DeclGroupSyntax>(
        _ node: Node
      ) throws -> [AttributeSyntax] {
        return try attachedMacro.expansion(
          of: customAttributeNode,
          attachedTo: node,
          providingAttributesFor: declarationNode,
          in: context
        )
      }

      let attributes = try _openExistential(
        parentDeclGroup, do: expandMemberAttributeMacro
      )
      // Form a buffer containing an attribute list to return to the caller.
      expanded = attributes
        .map { $0.trimmedDescription }
        .joined(separator: " ")

    case (let attachedMacro as MemberMacro.Type, .member):
      guard let declGroup = declarationNode.asProtocol(DeclGroupSyntax.self)
      else {
        fatalError("declNode for member macro must be DeclGroupSyntax")
      }

      // Local function to expand a member macro once we've opened up
      // the existential.
      func expandMemberMacro<Node: DeclGroupSyntax>(
        _ node: Node
      ) throws -> [DeclSyntax] {
        return try attachedMacro.expansion(
          of: customAttributeNode,
          providingMembersOf: node,
          in: context
        )
      }

      let members = try _openExistential(declGroup, do: expandMemberMacro)

      // Form a buffer of member declarations to return to the caller.
      expanded = members
        .map { $0.trimmedDescription }
        .joined(separator: "\n\n")

    case (let attachedMacro as PeerMacro.Type, .peer):
      let peers = try attachedMacro.expansion(
        of: customAttributeNode,
        providingPeersOf: declarationNode,
        in: context
      )

      // Form a buffer of peer declarations to return to the caller.
      expanded = peers
        .map { $0.trimmedDescription }
        .joined(separator: "\n\n")
    default:
      fatalError("\(macroDefinition) does not conform to any known attached macro protocol")
    }

    let diagnostics = context.diagnostics.map {
      PluginMessage.Diagnostic(from: $0, in: sourceManager)
    }
    try pluginHostConnection.sendMessage(
      .expandAttachedMacroResult(expandedSource: expanded, diagnostics: diagnostics))
  }

  private func getMacroDefinition(_ ref: PluginMessage.MacroReference) -> Any.Type? {
    let qualifedName = "\(ref.moduleName).\(ref.typeName)"

    for type in providingMarcos {
      // FIXME: getting the module name and type name should be more robust.
      let name = String(reflecting: type)
      if name == qualifedName {
        return type
      }
    }
    return nil
  }
}

/// Message channel for bidirectional communication with the plugin host.
internal fileprivate(set) var pluginHostConnection: PluginHostConnection!

typealias PluginHostConnection = MessageConnection<PluginToHostMessage, HostToPluginMessage>

internal struct MessageConnection<TX,RX> where TX: Encodable, RX: Decodable {
  let inputStream: FileHandle
  let outputStream: FileHandle

  func sendMessage(_ message: TX) throws {
    // Encode the message as JSON.
    let payload = try JSONEncoder().encode(message)

    // Write the header (a 64-bit length field in little endian byte order).
    var count = UInt64(littleEndian: UInt64(payload.count))
    let header = Swift.withUnsafeBytes(of: &count) { Data($0) }
    assert(header.count == 8)
    try outputStream.write(contentsOf: header)

    // Write the payload.
    try outputStream.write(contentsOf: payload)
  }

  func waitForNextMessage() throws -> RX? {
    // Read the header (a 64-bit length field in little endian byte order).
    guard let header = try inputStream.read(upToCount: 8) else { return nil }
    guard header.count == 8 else {
      throw PluginMessageError.truncatedHeader
    }

    // Decode the count.
    let count = header.withUnsafeBytes{ $0.load(as: UInt64.self).littleEndian }
    guard count >= 2 else {
      throw PluginMessageError.invalidPayloadSize
    }

    // Read the JSON payload.
    guard let payload = try inputStream.read(upToCount: Int(count)), payload.count == count else {
      throw PluginMessageError.truncatedPayload
    }

    // Decode and return the message.
    return try JSONDecoder().decode(RX.self, from: payload)
  }

  enum PluginMessageError: Swift.Error {
    case truncatedHeader
    case invalidPayloadSize
    case truncatedPayload
  }
}
