// swift-tools-version: 5.7
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
  name: "swift-macro-examples",
  platforms: [
    .macOS(.v11),
  ],
  products: [
    .executable(
      name: "MacroExamplesPlugin",
      targets: ["MacroExamplesPlugin"]),
//    .library(
//      name: "MacroExamplesLib",
//      targets: ["MacroExamplesLib"])
  ],
  dependencies: [
    .package(url: "https://github.com/apple/swift-syntax.git",
             branch: "main")
  ],
  targets: [
//    .target(
//      name: "MacroExamplesLib",
//      dependencies: [],
//      path: "MacroExamplesLib",
//      swiftSettings: [.unsafeFlags(["-enable-experimental-feature", "Macros"])]
//    ),
    .executableTarget(
      name: "MacroExamplesPlugin",
      dependencies: [
        "SwiftCompilerPlugin",
        .product(name: "SwiftSyntax", package: "swift-syntax"),
        .product(name: "SwiftDiagnostics", package: "swift-syntax"),
        .product(name: "SwiftOperators", package: "swift-syntax"),
      ],
      path: "./",
      sources: ["Sources/MacroExamplesPlugin", "MacroExamplesPlugin"]),

    .target(name: "SwiftCompilerPlugin",
            dependencies: [
              .product(name: "SwiftSyntax", package: "swift-syntax"),
              .product(name: "SwiftDiagnostics", package: "swift-syntax"),
              .product(name: "SwiftSyntaxMacros", package: "swift-syntax"),
            ]),
  ]
)
