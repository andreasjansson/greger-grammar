package tree_sitter_greger_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_greger "github.com/tree-sitter/tree-sitter-greger/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_greger.Language())
	if language == nil {
		t.Errorf("Error loading Greger grammar")
	}
}
