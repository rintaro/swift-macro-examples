import SwiftCompilerPlugin
import SwiftSyntaxMacros
import SwiftSyntax
import SwiftSyntaxBuilder

@main
struct MyCompilerPlugin: CompilerPlugin {
  var providingMarcos: [Any.Type] = [
    AddBlocker.self,
//    DictionaryStorageMacro.self,
    FontLiteralMacro.self,
    NewTypeMacro.self,
//    ObservableMacro.self,
    StringifyMacro.self,
    WarningMacro.self,
//    WrapStoredPropertiesMacro.self,
  ]
}

