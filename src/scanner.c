#include "tree_sitter/parser.h"
#include <wctype.h>
#include <string.h>
#include <stdio.h>

enum TokenType {
    TOOL_START_TAG,
    TOOL_END_TAG,
    TOOL_CONTENT_HEAD,
    TOOL_CONTENT_TAIL,
    HTML_COMMENT,
    EVAL_CONTENT,
    EVAL_RESULT_START_TAG,
    EVAL_RESULT_END_TAG,
    EVAL_RESULT_CONTENT_HEAD,
    EVAL_RESULT_CONTENT_TAIL,
    ERROR_SENTINEL,
    CODE_BACKTICKS,
    CODE_LANGUAGE_IDENTIFIER,
    CODE_CONTENTS,
    EVAL_LANGUAGE,
};

typedef struct {
    char tool_id[256];
    bool in_tool_content;
    bool expecting_tail; // true when we've scanned head and now need tail
    char eval_result_id[256];
    bool in_eval_result_content;
    bool expecting_eval_result_tail;
    int last_backtick_count; // Track the number of backticks in the opening sequence
} Scanner;

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = (Scanner *)payload;

    size_t tool_id_len = strlen(scanner->tool_id);
    if (tool_id_len >= 255) tool_id_len = 255;
    
    size_t eval_result_id_len = strlen(scanner->eval_result_id);
    if (eval_result_id_len >= 255) eval_result_id_len = 255;

    buffer[0] = scanner->in_tool_content ? 1 : 0;
    buffer[1] = scanner->expecting_tail ? 1 : 0;
    buffer[2] = tool_id_len;
    memcpy(buffer + 3, scanner->tool_id, tool_id_len);

    size_t offset = 3 + tool_id_len;
    buffer[offset] = scanner->in_eval_result_content ? 1 : 0;
    buffer[offset + 1] = scanner->expecting_eval_result_tail ? 1 : 0;
    buffer[offset + 2] = eval_result_id_len;
    memcpy(buffer + offset + 3, scanner->eval_result_id, eval_result_id_len);
    
    size_t final_offset = offset + 3 + eval_result_id_len;
    buffer[final_offset] = scanner->last_backtick_count;

    return final_offset + 1;
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    Scanner *scanner = (Scanner *)payload;

    if (length == 0) {
        scanner->in_tool_content = false;
        scanner->expecting_tail = false;
        scanner->tool_id[0] = '\0';
        scanner->in_eval_result_content = false;
        scanner->expecting_eval_result_tail = false;
        scanner->eval_result_id[0] = '\0';
        scanner->last_backtick_count = 0;
        return;
    }

    scanner->in_tool_content = buffer[0] == 1;
    if (length > 1) {
        scanner->expecting_tail = buffer[1] == 1;
        if (length > 2) {
            unsigned tool_id_len = buffer[2];
            if (tool_id_len >= 255) tool_id_len = 255;
            if (length >= 3 + tool_id_len) {
                memcpy(scanner->tool_id, buffer + 3, tool_id_len);
                scanner->tool_id[tool_id_len] = '\0';
                
                // Deserialize eval result state
                size_t offset = 3 + tool_id_len;
                if (length > offset) {
                    scanner->in_eval_result_content = buffer[offset] == 1;
                    if (length > offset + 1) {
                        scanner->expecting_eval_result_tail = buffer[offset + 1] == 1;
                        if (length > offset + 2) {
                            unsigned eval_result_id_len = buffer[offset + 2];
                            if (eval_result_id_len >= 255) eval_result_id_len = 255;
                            if (length >= offset + 3 + eval_result_id_len) {
                                memcpy(scanner->eval_result_id, buffer + offset + 3, eval_result_id_len);
                                scanner->eval_result_id[eval_result_id_len] = '\0';
                                
                                // Deserialize backtick count
                                size_t final_offset = offset + 3 + eval_result_id_len;
                                if (length > final_offset) {
                                    scanner->last_backtick_count = buffer[final_offset];
                                } else {
                                    scanner->last_backtick_count = 0;
                                }
                            } else {
                                scanner->eval_result_id[0] = '\0';
                                scanner->last_backtick_count = 0;
                            }
                        } else {
                            scanner->eval_result_id[0] = '\0';
                        }
                    } else {
                        scanner->expecting_eval_result_tail = false;
                        scanner->eval_result_id[0] = '\0';
                    }
                } else {
                    scanner->in_eval_result_content = false;
                    scanner->expecting_eval_result_tail = false;
                    scanner->eval_result_id[0] = '\0';
                }
            } else {
                scanner->tool_id[0] = '\0';
                scanner->in_eval_result_content = false;
                scanner->expecting_eval_result_tail = false;
                scanner->eval_result_id[0] = '\0';
            }
        } else {
            scanner->tool_id[0] = '\0';
            scanner->in_eval_result_content = false;
            scanner->expecting_eval_result_tail = false;
            scanner->eval_result_id[0] = '\0';
        }
    } else {
        scanner->expecting_tail = false;
        scanner->tool_id[0] = '\0';
        scanner->in_eval_result_content = false;
        scanner->expecting_eval_result_tail = false;
        scanner->eval_result_id[0] = '\0';
    }
}

void *tree_sitter_greger_external_scanner_create(void) {
    Scanner *scanner = malloc(sizeof(Scanner));
    scanner->in_tool_content = false;
    scanner->expecting_tail = false;
    scanner->tool_id[0] = '\0';
    scanner->in_eval_result_content = false;
    scanner->expecting_eval_result_tail = false;
    scanner->eval_result_id[0] = '\0';
    scanner->last_backtick_count = 0;
    return scanner;
}

void tree_sitter_greger_external_scanner_destroy(void *payload) {
    free(payload);
}

static bool scan_html_comment(TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    if (lexer->lookahead != '!') return false;
    advance(lexer);

    if (lexer->lookahead != '-') return false;
    advance(lexer);

    if (lexer->lookahead != '-') return false;
    advance(lexer);

    // Now we're inside the comment, scan until we find -->
    unsigned dashes = 0;
    while (lexer->lookahead) {
        switch (lexer->lookahead) {
            case '-':
                ++dashes;
                break;
            case '>':
                if (dashes >= 2) {
                    lexer->result_symbol = HTML_COMMENT;
                    advance(lexer);
                    return true;
                }
                // fallthrough
            default:
                dashes = 0;
        }
        advance(lexer);
    }

    return false;
}

static bool scan_tool_start_tag(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    // Check for "tool."
    if (lexer->lookahead != 't') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '.') return false;
    advance(lexer);

    // Get the tool ID and store it
    int id_len = 0;
    while (lexer->lookahead != '>' && lexer->lookahead != 0 && id_len < 255) {
        scanner->tool_id[id_len++] = lexer->lookahead;
        advance(lexer);
    }
    scanner->tool_id[id_len] = '\0';

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    scanner->in_tool_content = true;
    scanner->expecting_tail = false;
    lexer->result_symbol = TOOL_START_TAG;
    return true;
}

static bool scan_tool_end_tag(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    if (lexer->lookahead != '/') return false;
    advance(lexer);

    // Check for "tool."
    if (lexer->lookahead != 't') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'o') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '.') return false;
    advance(lexer);

    // Scan any tool ID until >
    while (lexer->lookahead != '>' && lexer->lookahead != 0) {
        advance(lexer);
    }

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    scanner->in_tool_content = false;
    scanner->expecting_tail = false;
    scanner->tool_id[0] = '\0';
    lexer->result_symbol = TOOL_END_TAG;
    return true;
}

static bool scan_tool_content_head(Scanner *scanner, TSLexer *lexer) {
    if (!scanner->in_tool_content || scanner->expecting_tail) return false;

    lexer->mark_end(lexer);

    // Build the expected closing tag
    char expected_closing[512];
    strncpy(expected_closing, "</tool.", sizeof(expected_closing) - 1);
    expected_closing[sizeof(expected_closing) - 1] = '\0';
    strncat(expected_closing, scanner->tool_id, sizeof(expected_closing) - strlen(expected_closing) - 1);
    strncat(expected_closing, ">", sizeof(expected_closing) - strlen(expected_closing) - 1);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;
    int line_count = 0;
    bool current_line_has_content = false;

    // Scan first 4 lines or until we find the closing tag
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == expected_closing[match_index]) {
            match_index++;
            if (match_index == expected_len) {
                // Found complete closing tag, stop here (don't consume it)
                if (has_content) {
                    lexer->result_symbol = TOOL_CONTENT_HEAD;
                    return true;
                } else {
                    // No content, let the grammar handle the end tag
                    return false;
                }
            }
            advance(lexer);
            current_line_has_content = true;
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching but failed, reset and process current char as content
                match_index = 0;
            }
            
            // Process current character as content
            if (lexer->lookahead == '\n') {
                if (current_line_has_content) {
                    line_count++;
                    current_line_has_content = false;
                }
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
                
                // If we've completed 4 lines, break and let natural flow determine if tail exists
                if (line_count >= 4) {
                    break;
                }
            } else {
                current_line_has_content = true;
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // Return head if we have content
    if (has_content) {
        // If we broke out because we reached 4 lines, check if there's more content
        if (line_count >= 4) {
            // Check if current position is at the closing tag
            int temp_match = 0;
            while (lexer->lookahead == expected_closing[temp_match] && temp_match < expected_len) {
                temp_match++;
                if (temp_match == expected_len) {
                    // We're at the closing tag, no tail needed
                    break;
                }
                lexer->advance(lexer, false);
            }
            
            if (temp_match != expected_len) {
                // We're not at the closing tag, so there must be more content
                scanner->expecting_tail = true;
            }
        }
        
        lexer->result_symbol = TOOL_CONTENT_HEAD;
        return true;
    }

    return false;
}

static bool scan_tool_content_tail(Scanner *scanner, TSLexer *lexer) {
    if (!scanner->in_tool_content || !scanner->expecting_tail) return false;

    lexer->mark_end(lexer);

    // Build the expected closing tag
    char expected_closing[512];
    strncpy(expected_closing, "</tool.", sizeof(expected_closing) - 1);
    expected_closing[sizeof(expected_closing) - 1] = '\0';
    strncat(expected_closing, scanner->tool_id, sizeof(expected_closing) - strlen(expected_closing) - 1);
    strncat(expected_closing, ">", sizeof(expected_closing) - strlen(expected_closing) - 1);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;

    // Scan remaining content until we find the closing tag
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == expected_closing[match_index]) {
            match_index++;
            if (match_index == expected_len) {
                // Found complete closing tag, stop here (don't consume it)
                scanner->expecting_tail = false;
                if (has_content) {
                    lexer->result_symbol = TOOL_CONTENT_TAIL;
                    return true;
                } else {
                    // No tail content, let the grammar handle the end tag
                    return false;
                }
            }
            advance(lexer);
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching, reset but don't advance yet
                match_index = 0;
                // Don't advance here, reprocess this character
            } else {
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // Reached end without finding closing tag
    scanner->expecting_tail = false;
    if (has_content) {
        lexer->result_symbol = TOOL_CONTENT_TAIL;
        return true;
    }

    return false;
}



static bool scan_eval_result_start_tag(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    // Check for "eval-result-"
    if (lexer->lookahead != 'e') return false;
    advance(lexer);
    if (lexer->lookahead != 'v') return false;
    advance(lexer);
    if (lexer->lookahead != 'a') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '-') return false;
    advance(lexer);
    if (lexer->lookahead != 'r') return false;
    advance(lexer);
    if (lexer->lookahead != 'e') return false;
    advance(lexer);
    if (lexer->lookahead != 's') return false;
    advance(lexer);
    if (lexer->lookahead != 'u') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != 't') return false;
    advance(lexer);
    if (lexer->lookahead != '-') return false;
    advance(lexer);

    // Get the eval result ID and store it
    int id_len = 0;
    while (lexer->lookahead != '>' && lexer->lookahead != 0 && id_len < 255) {
        scanner->eval_result_id[id_len++] = lexer->lookahead;
        advance(lexer);
    }
    scanner->eval_result_id[id_len] = '\0';

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    lexer->mark_end(lexer);
    scanner->in_eval_result_content = true;
    scanner->expecting_eval_result_tail = false;
    lexer->result_symbol = EVAL_RESULT_START_TAG;
    return true;
}

static bool scan_eval_result_end_tag(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '<') return false;
    advance(lexer);

    if (lexer->lookahead != '/') return false;
    advance(lexer);

    // Check for "eval-result-"
    if (lexer->lookahead != 'e') return false;
    advance(lexer);
    if (lexer->lookahead != 'v') return false;
    advance(lexer);
    if (lexer->lookahead != 'a') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != '-') return false;
    advance(lexer);
    if (lexer->lookahead != 'r') return false;
    advance(lexer);
    if (lexer->lookahead != 'e') return false;
    advance(lexer);
    if (lexer->lookahead != 's') return false;
    advance(lexer);
    if (lexer->lookahead != 'u') return false;
    advance(lexer);
    if (lexer->lookahead != 'l') return false;
    advance(lexer);
    if (lexer->lookahead != 't') return false;
    advance(lexer);
    if (lexer->lookahead != '-') return false;
    advance(lexer);

    // Scan eval result ID until >
    while (lexer->lookahead != '>' && lexer->lookahead != 0) {
        advance(lexer);
    }

    if (lexer->lookahead != '>') return false;
    advance(lexer);

    lexer->mark_end(lexer);
    scanner->in_eval_result_content = false;
    scanner->expecting_eval_result_tail = false;
    scanner->eval_result_id[0] = '\0';
    lexer->result_symbol = EVAL_RESULT_END_TAG;
    return true;
}

static bool scan_eval_result_content_head(Scanner *scanner, TSLexer *lexer) {
    if (!scanner->in_eval_result_content || scanner->expecting_eval_result_tail) return false;

    lexer->mark_end(lexer);

    // Build the expected closing tag
    char expected_closing[512];
    strncpy(expected_closing, "</eval-result-", sizeof(expected_closing) - 1);
    expected_closing[sizeof(expected_closing) - 1] = '\0';
    strncat(expected_closing, scanner->eval_result_id, sizeof(expected_closing) - strlen(expected_closing) - 1);
    strncat(expected_closing, ">", sizeof(expected_closing) - strlen(expected_closing) - 1);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;
    int line_count = 0;
    bool current_line_has_content = false;

    // Scan first 4 lines or until we find the closing tag
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == expected_closing[match_index]) {
            match_index++;
            if (match_index == expected_len) {
                // Found complete closing tag, stop here (don't consume it)
                lexer->result_symbol = EVAL_RESULT_CONTENT_HEAD;
                return true;
            }
            advance(lexer);
            current_line_has_content = true;
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching but failed, reset and process current char as content
                match_index = 0;
            }
            
            // Process current character as content
            if (lexer->lookahead == '\n') {
                if (current_line_has_content) {
                    line_count++;
                    current_line_has_content = false;
                }
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
                
                // If we've completed 4 lines, break and let natural flow determine if tail exists
                if (line_count >= 4) {
                    break;
                }
            } else {
                current_line_has_content = true;
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // Always return head token (even for empty content, like tool_content_head)
    if (has_content) {
        // If we broke out because we reached 4 lines, check if there's more content
        if (line_count >= 4) {
            // Check if current position is at the closing tag
            int temp_match = 0;
            while (lexer->lookahead == expected_closing[temp_match] && temp_match < expected_len) {
                temp_match++;
                if (temp_match == expected_len) {
                    // We're at the closing tag, no tail needed
                    break;
                }
                lexer->advance(lexer, false);
            }
            
            if (temp_match != expected_len) {
                // We're not at the closing tag, so there must be more content
                scanner->expecting_eval_result_tail = true;
            }
        }
    }
    
    lexer->result_symbol = EVAL_RESULT_CONTENT_HEAD;
    return true;

    return false;
}

static bool scan_eval_result_content_tail(Scanner *scanner, TSLexer *lexer) {
    if (!scanner->in_eval_result_content || !scanner->expecting_eval_result_tail) return false;

    lexer->mark_end(lexer);

    // Build the expected closing tag
    char expected_closing[512];
    strncpy(expected_closing, "</eval-result-", sizeof(expected_closing) - 1);
    expected_closing[sizeof(expected_closing) - 1] = '\0';
    strncat(expected_closing, scanner->eval_result_id, sizeof(expected_closing) - strlen(expected_closing) - 1);
    strncat(expected_closing, ">", sizeof(expected_closing) - strlen(expected_closing) - 1);
    int expected_len = strlen(expected_closing);

    int match_index = 0;
    bool has_content = false;

    // Scan remaining content until we find the closing tag
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == expected_closing[match_index]) {
            match_index++;
            if (match_index == expected_len) {
                // Found complete closing tag, stop here (don't consume it)
                scanner->expecting_eval_result_tail = false;
                if (has_content) {
                    lexer->result_symbol = EVAL_RESULT_CONTENT_TAIL;
                    return true;
                } else {
                    // No tail content, let the grammar handle the end tag
                    return false;
                }
            }
            advance(lexer);
        } else {
            // Reset match and continue as content
            if (match_index > 0) {
                // We were partially matching, reset but don't advance yet
                match_index = 0;
                // Don't advance here, reprocess this character
            } else {
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            }
        }
    }

    // Reached end without finding closing tag
    scanner->expecting_eval_result_tail = false;
    if (has_content) {
        lexer->result_symbol = EVAL_RESULT_CONTENT_TAIL;
        return true;
    }

    return false;
}





static bool scan_code_backticks(Scanner *scanner, TSLexer *lexer) {
    if (lexer->lookahead != '`') return false;
    
    // Count and consume backticks (any number, 1 or more)
    int backtick_count = 0;
    while (lexer->lookahead == '`') {
        backtick_count++;
        advance(lexer);
    }
    
    if (backtick_count >= 1) {
        // If this is the first time we're seeing backticks, store the count
        // If we already have a count, this is likely the closing sequence
        if (scanner->last_backtick_count == 0) {
            scanner->last_backtick_count = backtick_count;
        } else {
            // This is a closing sequence, reset the count
            scanner->last_backtick_count = 0;
        }
        
        lexer->result_symbol = CODE_BACKTICKS;
        return true;
    }
    
    return false;
}

static bool scan_code_language(Scanner *scanner, TSLexer *lexer) {
    printf("DEBUG: scan_code_language called, backtick_count:%d, char:'%c'\n", 
           scanner->last_backtick_count, lexer->lookahead);
    
    // For single backtick blocks, don't return language token
    if (scanner->last_backtick_count == 1) {
        printf("DEBUG: scan_code_language returning false (single backtick)\n");
        return false;
    }
    
    // For multi-backtick blocks (2+), only return language token if there's actual valid language content
    TSLexer saved_lexer = *lexer;
    
    // Skip any leading whitespace
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        advance(lexer);
    }
    
    // If we hit a newline immediately, there's no language
    if (lexer->lookahead == '\n' || lexer->lookahead == 0) {
        return false;
    }
    
    // Check if it's a valid language identifier start
    if (!iswlower(lexer->lookahead) && !iswupper(lexer->lookahead) && lexer->lookahead != '_') {
        // Invalid language start - no language token
        return false;
    }
    
    // Scan the potential language identifier
    bool has_content = false;
    
    while (iswlower(lexer->lookahead) || iswupper(lexer->lookahead) || 
           iswdigit(lexer->lookahead) || lexer->lookahead == '_' || 
           lexer->lookahead == '+' || lexer->lookahead == '-') {
        advance(lexer);
        has_content = true;
    }
    
    if (!has_content) {
        // No valid language content
        return false;
    }
    
    // Check what comes after the identifier
    // Language must be followed by newline, EOF, or spaces then newline/EOF
    if (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        // Skip spaces
        while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
            advance(lexer);
        }
    }
    
    // After the identifier (and any trailing spaces), we must see newline or EOF
    if (lexer->lookahead != '\n' && lexer->lookahead != 0) {
        // Invalid language (has spaces or other characters) - no language token
        return false;
    }
    
    // If we get here, we have a valid language identifier
    *lexer = saved_lexer; // Restore to start of language
    
    // Skip any leading whitespace
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        advance(lexer);
    }
    
    // Re-scan and consume the language
    while (iswlower(lexer->lookahead) || iswupper(lexer->lookahead) || 
           iswdigit(lexer->lookahead) || lexer->lookahead == '_' || 
           lexer->lookahead == '+' || lexer->lookahead == '-') {
        advance(lexer);
    }
    
    lexer->result_symbol = CODE_LANGUAGE_IDENTIFIER;
    return true;
}

static bool scan_code_contents(Scanner *scanner, TSLexer *lexer) {
    printf("DEBUG: scan_code_contents called, backtick_count:%d, char:'%c'\n", 
           scanner->last_backtick_count, lexer->lookahead);
    
    bool has_content = false;
    
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == '`') {
            // Check if this is a backtick sequence that matches the opening count
            TSLexer saved_lexer = *lexer;
            int backtick_count = 0;
            
            // Count consecutive backticks
            while (lexer->lookahead == '`') {
                backtick_count++;
                advance(lexer);
            }
            
            // If this matches the opening backtick count, this is the closing sequence
            if (backtick_count == scanner->last_backtick_count) {
                // Restore lexer position and let the grammar handle the closing backticks
                *lexer = saved_lexer;
                break;
            } else {
                // This is not the closing sequence, include it as content
                // Restore and consume the backticks as content
                *lexer = saved_lexer;
                for (int i = 0; i < backtick_count; i++) {
                    advance(lexer);
                    has_content = true;
                }
                lexer->mark_end(lexer);
            }
        } else {
            advance(lexer);
            has_content = true;
            lexer->mark_end(lexer);
        }
    }
    
    // Always return CODE_CONTENTS token, even for empty content
    lexer->result_symbol = CODE_CONTENTS;
    return true;
}

static bool scan_eval_language(TSLexer *lexer) {
    // Skip any leading whitespace
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
        advance(lexer);
    }
    
    // Must start with a letter or underscore for valid language
    if (!iswlower(lexer->lookahead) && !iswupper(lexer->lookahead) && lexer->lookahead != '_') {
        return false;
    }
    
    // Scan the language identifier
    bool has_content = false;
    
    while (iswlower(lexer->lookahead) || iswupper(lexer->lookahead) || 
           iswdigit(lexer->lookahead) || lexer->lookahead == '_' || 
           lexer->lookahead == '+' || lexer->lookahead == '-') {
        advance(lexer);
        has_content = true;
    }
    
    if (!has_content) {
        return false;
    }
    
    lexer->result_symbol = EVAL_LANGUAGE;
    return true;
}


static bool scan_eval_content(TSLexer *lexer) {
    bool has_content = false;
    bool has_non_whitespace = false;
    int brace_count = 0;
    
    // Don't consume content that starts with : (language prefix)
    if (lexer->lookahead == ':') {
        return false;
    }
    
    while (lexer->lookahead != 0) {
        if (lexer->lookahead == '{') {
            // Found opening brace, increment counter
            brace_count++;
            if (!iswspace(lexer->lookahead)) {
                has_non_whitespace = true;
            }
            advance(lexer);
            has_content = true;
            lexer->mark_end(lexer);
        } else if (lexer->lookahead == '}') {
            if (brace_count > 0) {
                // This is a closing brace for a nested opening brace
                brace_count--;
                if (!iswspace(lexer->lookahead)) {
                    has_non_whitespace = true;
                }
                advance(lexer);
                has_content = true;
                lexer->mark_end(lexer);
            } else {
                // This is the closing brace for our eval, stop here
                break;
            }
        } else if (lexer->lookahead == '<') {
            TSLexer saved = *lexer;
            advance(lexer);
            
            // Check if this is a tag starting with 'e' (could be eval-result)
            if (lexer->lookahead == 'e') {
                *lexer = saved;
                break; // Exit the while loop
            }
            
            // Otherwise, restore and continue as content
            *lexer = saved;
            if (!iswspace(lexer->lookahead)) {
                has_non_whitespace = true;
            }
            advance(lexer);
            has_content = true;
            lexer->mark_end(lexer);
        } else {
            if (!iswspace(lexer->lookahead)) {
                has_non_whitespace = true;
            }
            advance(lexer);
            has_content = true;
            lexer->mark_end(lexer);
        }
    }
    
    if (has_content && has_non_whitespace) {
        lexer->result_symbol = EVAL_CONTENT;
        return true;
    }
    
    return false;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;

    // Debug: Print valid symbols for code-related tokens
    if (valid_symbols[CODE_BACKTICKS] || valid_symbols[CODE_LANGUAGE_IDENTIFIER] || valid_symbols[CODE_CONTENTS]) {
        printf("DEBUG: valid_symbols - backticks:%d, language:%d, contents:%d, char:'%c'\n", 
               valid_symbols[CODE_BACKTICKS], valid_symbols[CODE_LANGUAGE_IDENTIFIER], 
               valid_symbols[CODE_CONTENTS], lexer->lookahead);
    }

    // Check for error recovery mode
    if (valid_symbols[ERROR_SENTINEL]) {
        return false;
    }

    // Handle tool content (raw text) when in tool content state
    if (scanner->in_tool_content) {
        if (valid_symbols[TOOL_CONTENT_HEAD] && !scanner->expecting_tail) {
            return scan_tool_content_head(scanner, lexer);
        }
        if (valid_symbols[TOOL_CONTENT_TAIL] && scanner->expecting_tail) {
            return scan_tool_content_tail(scanner, lexer);
        }
    }

    // Handle eval result content when in eval result content state
    if (scanner->in_eval_result_content) {
        if (valid_symbols[EVAL_RESULT_CONTENT_HEAD] && !scanner->expecting_eval_result_tail) {
            return scan_eval_result_content_head(scanner, lexer);
        }
        if (valid_symbols[EVAL_RESULT_CONTENT_TAIL] && scanner->expecting_eval_result_tail) {
            return scan_eval_result_content_tail(scanner, lexer);
        }
    }

    // Skip whitespace but preserve newlines for other tokens
    // Don't skip whitespace for code contents as spaces are part of the content
    if (!valid_symbols[CODE_CONTENTS]) {
        while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
            skip(lexer);
        }
    }



    if (lexer->lookahead == '<') {
        // Handle HTML comments first - they should have priority
        if (valid_symbols[HTML_COMMENT]) {
            return scan_html_comment(lexer);
        }

        // Handle eval result start tag
        if (valid_symbols[EVAL_RESULT_START_TAG]) {
            return scan_eval_result_start_tag(scanner, lexer);
        }

        // Handle eval result end tag
        if (valid_symbols[EVAL_RESULT_END_TAG]) {
            return scan_eval_result_end_tag(scanner, lexer);
        }

        // Handle tool start tag
        if (valid_symbols[TOOL_START_TAG]) {
            return scan_tool_start_tag(scanner, lexer);
        }

        // Handle tool end tag
        if (valid_symbols[TOOL_END_TAG]) {
            return scan_tool_end_tag(scanner, lexer);
        }
    }

    // Handle eval content
    if (valid_symbols[EVAL_CONTENT]) {
        return scan_eval_content(lexer);
    }
    
    // Handle code backticks (any number of backticks)
    if (valid_symbols[CODE_BACKTICKS] && lexer->lookahead == '`') {
        return scan_code_backticks(scanner, lexer);
    }
    
    // Handle code language identifier
    if (valid_symbols[CODE_LANGUAGE_IDENTIFIER]) {
        return scan_code_language(scanner, lexer);
    }
    
    // Handle code contents
    if (valid_symbols[CODE_CONTENTS]) {
        return scan_code_contents(scanner, lexer);
    }
    
    // Handle eval language
    if (valid_symbols[EVAL_LANGUAGE]) {
        return scan_eval_language(lexer);
    }
    
    return false;
}


