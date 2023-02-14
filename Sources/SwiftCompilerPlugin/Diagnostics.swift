import SwiftDiagnostics
import SwiftSyntax

/// Errors in macro handing.
enum MacroExpansionError: String {
  case macroTypeNotFound = "macro expanding type not found"
  case freestandingMacroSyntaxIsNotMacro = "macro syntax couldn't be parsed"
}

extension MacroExpansionError: DiagnosticMessage {
  var message: String {
    self.rawValue
  }
  var diagnosticID: SwiftDiagnostics.MessageID {
    .init(domain: "\(type(of: self))", id: "\(self)")
  }
  var severity: SwiftDiagnostics.DiagnosticSeverity {
    .error
  }
}

extension PluginMessage.Diagnostic.Severity {
  init(from syntaxDiagSeverity: SwiftDiagnostics.DiagnosticSeverity) {
    switch syntaxDiagSeverity {
    case .error: self = .error
    case .warning: self = .warning
    case .note: self = .note
    }
  }
}

extension PluginMessage.Diagnostic {
  init(from syntaxDiag: SwiftDiagnostics.Diagnostic, in sourceManager: SourceManager) {
    guard let position =  sourceManager.position(
      of: syntaxDiag.node, at: .afterLeadingTrivia) else {
      fatalError("unknown diagnostic node")
    }

    self.position = .init(fileName: position.fileName, offset: position.utf8Offset)
    self.severity = .init(from: syntaxDiag.diagMessage.severity)
    self.message = syntaxDiag.message

    self.highlights = syntaxDiag.highlights.map {
      guard let range = sourceManager.range(of: $0) else {
        fatalError("highlight node is not known")
      }
      return .init(fileName: range.fileName,
                   startOffset: range.startUTF8Offset,
                   endOffset: range.endUTF8Offset)
    }

    self.notes = syntaxDiag.notes.map {
      guard let pos = sourceManager.position(of: $0.node, at: .afterLeadingTrivia) else {
        fatalError("note node is not known")
      }
      let position = PluginMessage.Diagnostic.Position(
        fileName: pos.fileName, offset: pos.utf8Offset)
      return .init(position: position, message: $0.message)
    }

    self.fixIts = syntaxDiag.fixIts.map {
      PluginMessage.Diagnostic.FixIt(
        message: $0.message.message,
        changes: $0.changes.changes.map {
          let range: SourceManager.SourceRange?
          let text: String
          switch $0 {
          case .replace(let oldNode, let newNode):
            range = sourceManager.range(
              of: oldNode, from: .afterLeadingTrivia, to: .beforeTrailingTrivia)
            text = newNode.trimmedDescription
          case .replaceLeadingTrivia(let token, let newTrivia):
            range = sourceManager.range(
              of: Syntax(token), from: .beforeLeadingTrivia, to: .afterLeadingTrivia)
            text = newTrivia.description
          case .replaceTrailingTrivia(let token, let newTrivia):
            range = sourceManager.range(
              of: Syntax(token), from: .beforeTrailingTrivia, to: .afterTrailingTrivia)
            text = newTrivia.description
          }
          guard let range = range else {
            fatalError("unknown")
          }
          return .init(
            range: PositionRange(fileName: range.fileName,
                                 startOffset: range.startUTF8Offset,
                                 endOffset: range.endUTF8Offset),
            newText: text)
        })
    }
  }
}
