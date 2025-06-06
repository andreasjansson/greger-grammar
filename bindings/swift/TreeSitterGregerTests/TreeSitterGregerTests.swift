import XCTest
import SwiftTreeSitter
import TreeSitterGreger

final class TreeSitterGregerTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_greger())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Greger grammar")
    }
}
