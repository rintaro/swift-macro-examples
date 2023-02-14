import SwiftCompilerPlugin
import SwiftSyntaxMacros
import SwiftSyntax
import SwiftSyntaxBuilder

@main
struct MyCompilerPlugin: CompilerPlugin {
  var providingMarcos: [Macro.Type] = [
    AddBlocker.self,
    DictionaryStorageMacro.self,
    FontLiteralMacro.self,
    NewTypeMacro.self,
    ObservableMacro.self,
    ObservablePropertyMacro.self,
    StringifyMacro.self,
    WarningMacro.self,
    WrapStoredPropertiesMacro.self,
    AddCompletionHandlerMacro.self,
  ]
}

