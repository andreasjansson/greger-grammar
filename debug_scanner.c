#include "tree_sitter/parser.h"
#include <wctype.h>
#include <string.h>
#include <stdio.h>

// Debug function to print valid symbols
void debug_valid_symbols(const bool *valid_symbols) {
    const char *symbol_names[] = {
        "TOOL_START_TAG",
        "TOOL_END_TAG", 
        "TOOL_CONTENT_HEAD",
        "TOOL_CONTENT_TAIL",
        "HTML_COMMENT",
        "EVAL_CONTENT",
        "EVAL_RESULT_START_TAG",
        "EVAL_RESULT_END_TAG",
        "EVAL_RESULT_CONTENT_HEAD",
        "EVAL_RESULT_CONTENT_TAIL",
        "ERROR_SENTINEL",
        "CODE_START_TAG",
        "CODE_CONTENT",
        "CODE_END_TAG",
        "EVAL_LANGUAGE",
    };
    
    fprintf(stderr, "Valid symbols: ");
    for (int i = 0; i < 15; i++) {
        if (valid_symbols[i]) {
            fprintf(stderr, "%s ", symbol_names[i]);
        }
    }
    fprintf(stderr, "\n");
}
