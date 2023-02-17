import SwiftDiagnostics
import SwiftSyntax
import SwiftSyntaxMacros

extension CompilerPlugin {
  func expandFreestandingMacro(
    macro: PluginMessage.MacroReference,
    discriminator: String,
    expandingSyntax: PluginMessage.Syntax
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

  func expandAttachedMacro(
    macro: PluginMessage.MacroReference,
    macroRole: PluginMessage.MacroRole,
    discriminator: String,
    customAttributeSyntax: PluginMessage.Syntax,
    declSyntax: PluginMessage.Syntax,
    parentDeclSyntax: PluginMessage.Syntax?
  ) throws {
    let sourceManager = SourceManager()
    let context = PluginMacroExpansionContext(
      sourceManager: sourceManager, discriminator: discriminator)

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

  private func getMacroDefinition(_ ref: PluginMessage.MacroReference) -> Macro.Type? {
    let qualifedName = "\(ref.moduleName).\(ref.typeName)"

    for type in self.providingMarcos {
      // FIXME: getting the module name and type name should be more robust.
      let name = String(reflecting: type)
      if name == qualifedName {
        return type
      }
    }
    return nil
  }
}
