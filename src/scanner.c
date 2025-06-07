#include <tree_sitter/parser.h>
#include <tree_sitter/alloc.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  TOOL_BLOCK_START,
  TOOL_BLOCK_END,
  TOOL_BLOCK_CONTENT,
  INLINE_CODE,
  CODE_FENCE_START,
  CODE_FENCE_END,
  CODE_FENCE_CONTENT,
  NEWLINE,
  EOF_TOKEN,
};

typedef struct {
  char current_tool_id[64];
  bool in_tool_block;
  bool in_code_fence;
  int code_fence_length;
  char code_fence_char; // '`' or '~'
} Scanner;

static void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

void *tree_sitter_greger_external_scanner_create() {
  Scanner *scanner = ts_malloc(sizeof(Scanner));
  scanner->current_tool_id[0] = '\0';
  scanner->in_tool_block = false;
  scanner->in_code_fence = false;
  scanner->code_fence_length = 0;
  scanner->code_fence_char = 0;
  return scanner;
}

void tree_sitter_greger_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  ts_free(scanner);
}

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  size_t id_len = strlen(scanner->current_tool_id);

  buffer[0] = scanner->in_tool_block ? 1 : 0;
  buffer[1] = scanner->in_code_fence ? 1 : 0;
  buffer[2] = (char)scanner->code_fence_length;
  buffer[3] = scanner->code_fence_char;
  buffer[4] = (char)id_len;
  memcpy(buffer + 5, scanner->current_tool_id, id_len);

  return 5 + id_len;
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *scanner = (Scanner *)payload;

  if (length < 5) {
    scanner->in_tool_block = false;
    scanner->in_code_fence = false;
    scanner->code_fence_length = 0;
    scanner->code_fence_char = 0;
    scanner->current_tool_id[0] = '\0';
    return;
  }

  scanner->in_tool_block = buffer[0] == 1;
  scanner->in_code_fence = buffer[1] == 1;
  scanner->code_fence_length = (int)buffer[2];
  scanner->code_fence_char = buffer[3];
  size_t id_len = (size_t)buffer[4];

  if (length >= 5 + id_len && id_len < 63) {
    memcpy(scanner->current_tool_id, buffer + 5, id_len);
    scanner->current_tool_id[id_len] = '\0';
  } else {
    scanner->current_tool_id[0] = '\0';
  }
}

static bool scan_newline(TSLexer *lexer) {
  if (lexer->lookahead == '\n') {
    advance(lexer);
    lexer->result_symbol = NEWLINE;
    return true;
  }
  return false;
}

static bool scan_eof(TSLexer *lexer) {
  if (lexer->lookahead == 0) {
    lexer->result_symbol = EOF_TOKEN;
    return true;
  }
  return false;
}

static bool scan_inline_code(TSLexer *lexer) {
  if (lexer->lookahead != '`') return false;

  advance(lexer); // consume first `

  // Look for closing `
  while (lexer->lookahead && lexer->lookahead != '`' && lexer->lookahead != '\n') {
    advance(lexer);
  }

  if (lexer->lookahead == '`') {
    advance(lexer); // consume closing `
    lexer->result_symbol = INLINE_CODE;
    return true;
  }

  return false;
}

static bool scan_code_fence_start(TSLexer *lexer, Scanner *scanner) {
  if (scanner->in_code_fence) return false;

  char fence_char = lexer->lookahead;
  if (fence_char != '`' && fence_char != '~') return false;

  int count = 0;
  while (lexer->lookahead == fence_char) {
    advance(lexer);
    count++;
  }

  if (count >= 3) {
    scanner->in_code_fence = true;
    scanner->code_fence_length = count;
    scanner->code_fence_char = fence_char;
    lexer->result_symbol = CODE_FENCE_START;
    return true;
  }

  return false;
}

static bool scan_code_fence_end(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_code_fence) return false;

  char fence_char = lexer->lookahead;
  if (fence_char != scanner->code_fence_char) return false;

  int count = 0;
  while (lexer->lookahead == fence_char) {
    advance(lexer);
    count++;
  }

  if (count >= scanner->code_fence_length) {
    scanner->in_code_fence = false;
    scanner->code_fence_length = 0;
    scanner->code_fence_char = 0;
    lexer->result_symbol = CODE_FENCE_END;
    return true;
  }

  return false;
}

static bool scan_code_fence_content(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_code_fence) return false;

  bool has_content = false;

  while (lexer->lookahead && lexer->lookahead != '\n') {
    // Check if this might be a closing fence
    if (lexer->lookahead == scanner->code_fence_char) {
      // Look ahead to see if this is a closing fence
      lexer->mark_end(lexer);

      int count = 0;
      while (lexer->lookahead == scanner->code_fence_char) {
        advance(lexer);
        count++;
      }

      if (count >= scanner->code_fence_length &&
          (lexer->lookahead == '\n' || lexer->lookahead == 0 || iswspace(lexer->lookahead))) {
        // This is the closing fence, stop here
        break;
      }

      // Not a closing fence, continue
      has_content = true;
    } else {
      advance(lexer);
      has_content = true;
    }
  }

  if (has_content) {
    lexer->result_symbol = CODE_FENCE_CONTENT;
    return true;
  }

  return false;
}

// Check if the current position looks like our closing tag without advancing
static bool lookahead_for_closing_tag(TSLexer *lexer, Scanner *scanner) {
  // Save the current position
  lexer->mark_end(lexer);

  // Check if we have "</tool.ID>"
  if (lexer->lookahead != '<') return false;
  advance(lexer);

  if (lexer->lookahead != '/') return false;
  advance(lexer);

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

  // Read the tool ID and check if it matches our current one
  char end_id[64];
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    end_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;

  end_id[id_len] = '\0';

  // Reset position back to the mark (the '<')
  return strcmp(scanner->current_tool_id, end_id) == 0;
}

static bool scan_tool_start(TSLexer *lexer, Scanner *scanner) {
  // Expect "<tool."
  if (lexer->lookahead != '<') return false;
  advance(lexer);

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

  // Read the tool ID
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    scanner->current_tool_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;
  advance(lexer);

  scanner->current_tool_id[id_len] = '\0';
  scanner->in_tool_block = true;

  lexer->result_symbol = TOOL_BLOCK_START;
  return true;
}

static bool scan_tool_end(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_tool_block) return false;

  // Expect "</tool."
  if (lexer->lookahead != '<') return false;
  advance(lexer);

  if (lexer->lookahead != '/') return false;
  advance(lexer);

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

  // Read the tool ID and check if it matches our current one
  char end_id[64];
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    end_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;
  advance(lexer);

  end_id[id_len] = '\0';

  // Only match if the IDs are the same as our current tool ID
  if (strcmp(scanner->current_tool_id, end_id) == 0) {
    scanner->in_tool_block = false;
    scanner->current_tool_id[0] = '\0';
    lexer->result_symbol = TOOL_BLOCK_END;
    return true;
  }

  return false;
}

static bool scan_tool_content(TSLexer *lexer, Scanner *scanner) {
  if (!scanner->in_tool_block) return false;

  bool has_content = false;

  // Consume characters until we see our closing tag
  while (lexer->lookahead) {
    if (lexer->lookahead == '<' && lookahead_for_closing_tag(lexer, scanner)) {
      // We've hit our closing tag, stop consuming content
      break;
    }
    advance(lexer);
    has_content = true;
  }

  if (has_content) {
    lexer->result_symbol = TOOL_BLOCK_CONTENT;
    return true;
  }

  return false;
}

bool tree_sitter_greger_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  // Handle EOF
  if (valid_symbols[EOF_TOKEN] && scan_eof(lexer)) {
    return true;
  }

  // Handle newlines
  if (valid_symbols[NEWLINE] && scan_newline(lexer)) {
    return true;
  }

  // Try to handle tool block end tokens first when we're in a tool block
  if (valid_symbols[TOOL_BLOCK_END] && scanner->in_tool_block) {
    // Skip whitespace for end tokens
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
      skip(lexer);
    }
    if (scan_tool_end(lexer, scanner)) {
      return true;
    }
  }

  // Handle tool block content (don't skip whitespace for content)
  if (valid_symbols[TOOL_BLOCK_CONTENT] && scanner->in_tool_block) {
    return scan_tool_content(lexer, scanner);
  }

  // Handle code fence end
  if (valid_symbols[CODE_FENCE_END] && scanner->in_code_fence) {
    // Skip whitespace at start of line
    while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
      skip(lexer);
    }
    if (scan_code_fence_end(lexer, scanner)) {
      return true;
    }
  }

  // Handle code fence content
  if (valid_symbols[CODE_FENCE_CONTENT] && scanner->in_code_fence) {
    return scan_code_fence_content(lexer, scanner);
  }

  // Handle inline code
  if (valid_symbols[INLINE_CODE] && !scanner->in_code_fence && !scanner->in_tool_block) {
    if (scan_inline_code(lexer)) {
      return true;
    }
  }

  // Skip whitespace for other tokens (except newlines)
  while (iswspace(lexer->lookahead) && lexer->lookahead != '\n') {
    skip(lexer);
  }

  // Handle tool block start
  if (valid_symbols[TOOL_BLOCK_START] && !scanner->in_tool_block && !scanner->in_code_fence) {
    if (scan_tool_start(lexer, scanner)) {
      return true;
    }
  }

  // Handle code fence start
  if (valid_symbols[CODE_FENCE_START] && !scanner->in_code_fence && !scanner->in_tool_block) {
    if (scan_code_fence_start(lexer, scanner)) {
      return true;
    }
  }

  return false;
}
