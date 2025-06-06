#include <tree_sitter/parser.h>
#include <tree_sitter/array.h>
#include <tree_sitter/alloc.h>
#include <string.h>
#include <wctype.h>

enum TokenType {
  TOOL_BLOCK_START,
  TOOL_BLOCK_END,
  TOOL_BLOCK_CONTENT,
};

typedef struct {
  Array(char*) *tool_ids;  // Stack of tool IDs for nested matching
} Scanner;

static void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

void *tree_sitter_greger_external_scanner_create() {
  Scanner *scanner = ts_malloc(sizeof(Scanner));
  scanner->tool_ids = ts_malloc(sizeof(Array(char*)));
  array_init(scanner->tool_ids);
  return scanner;
}

void tree_sitter_greger_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  for (size_t i = 0; i < scanner->tool_ids->size; ++i) {
    ts_free(*array_get(scanner->tool_ids, i));
  }
  array_delete(scanner->tool_ids);
  ts_free(scanner);
}

unsigned tree_sitter_greger_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  unsigned size = 0;

  // Write number of tool IDs
  buffer[size++] = (char)scanner->tool_ids->size;

  // Write each tool ID
  for (size_t i = 0; i < scanner->tool_ids->size; ++i) {
    char *tool_id = *array_get(scanner->tool_ids, i);
    size_t id_len = strlen(tool_id);
    buffer[size++] = (char)id_len;
    memcpy(buffer + size, tool_id, id_len);
    size += id_len;
  }

  return size;
}

void tree_sitter_greger_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *scanner = (Scanner *)payload;

  // Clear existing data
  for (size_t i = 0; i < scanner->tool_ids->size; ++i) {
    ts_free(*array_get(scanner->tool_ids, i));
  }
  array_init(scanner->tool_ids);

  if (length == 0) return;

  unsigned pos = 0;
  size_t count = (size_t)buffer[pos++];

  for (size_t i = 0; i < count && pos < length; ++i) {
    size_t id_len = (size_t)buffer[pos++];
    if (pos + id_len <= length) {
      char *tool_id = ts_malloc(id_len + 1);
      memcpy(tool_id, buffer + pos, id_len);
      tool_id[id_len] = '\0';
      array_push(scanner->tool_ids, tool_id);
      pos += id_len;
    }
  }
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
  char tool_id[64];
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    tool_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  if (lexer->lookahead != '>') return false;
  advance(lexer);

  tool_id[id_len] = '\0';

  // Store the tool ID on the stack
  char *stored_id = ts_malloc(id_len + 1);
  strcpy(stored_id, tool_id);
  array_push(scanner->tool_ids, stored_id);

  lexer->result_symbol = TOOL_BLOCK_START;
  return true;
}

static bool scan_tool_end(TSLexer *lexer, Scanner *scanner) {
  if (scanner->tool_ids->size == 0) return false;

  // Get the current tool ID from the top of the stack
  char *current_id = *array_get(scanner->tool_ids, scanner->tool_ids->size - 1);

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

  // Read the tool ID and check if it matches
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

  // Only match if the IDs are the same
  if (strcmp(current_id, end_id) == 0) {
    // Pop the tool ID from the stack
    ts_free(array_pop(scanner->tool_ids));
    lexer->result_symbol = TOOL_BLOCK_END;
    return true;
  }

  return false;
}

static bool is_closing_tag(TSLexer *lexer, Scanner *scanner) {
  if (scanner->tool_ids->size == 0) return false;
  if (lexer->lookahead != '<') return false;

  // Save lexer state for lookahead
  TSLexer saved_lexer = *lexer;

  advance(lexer);
  if (lexer->lookahead != '/') {
    *lexer = saved_lexer;
    return false;
  }

  advance(lexer);
  if (lexer->lookahead != 't') {
    *lexer = saved_lexer;
    return false;
  }

  advance(lexer);
  if (lexer->lookahead != 'o') {
    *lexer = saved_lexer;
    return false;
  }

  advance(lexer);
  if (lexer->lookahead != 'o') {
    *lexer = saved_lexer;
    return false;
  }

  advance(lexer);
  if (lexer->lookahead != 'l') {
    *lexer = saved_lexer;
    return false;
  }

  advance(lexer);
  if (lexer->lookahead != '.') {
    *lexer = saved_lexer;
    return false;
  }

  advance(lexer);

  // Check if the ID matches our current tool ID
  char *current_id = *array_get(scanner->tool_ids, scanner->tool_ids->size - 1);

  char end_id[64];
  int id_len = 0;
  while (lexer->lookahead &&
         (isalnum(lexer->lookahead) || lexer->lookahead == '_' ||
          lexer->lookahead == '-' || lexer->lookahead == '.') &&
         id_len < 63) {
    end_id[id_len++] = lexer->lookahead;
    advance(lexer);
  }

  end_id[id_len] = '\0';

  bool matches = (lexer->lookahead == '>' && strcmp(current_id, end_id) == 0);

  // Restore lexer state
  *lexer = saved_lexer;

  return matches;
}

static bool scan_tool_content(TSLexer *lexer, Scanner *scanner) {
  if (scanner->tool_ids->size == 0) return false;

  bool has_content = false;

  // Scan everything until we find the matching closing tag
  while (lexer->lookahead) {
    if (is_closing_tag(lexer, scanner)) {
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

  // Handle tool block content first (don't skip whitespace for content)
  if (valid_symbols[TOOL_BLOCK_CONTENT] && scanner->tool_ids->size > 0) {
    return scan_tool_content(lexer, scanner);
  }

  // Skip whitespace for start/end tokens
  while (iswspace(lexer->lookahead)) {
    skip(lexer);
  }

  if (valid_symbols[TOOL_BLOCK_START]) {
    return scan_tool_start(lexer, scanner);
  }

  if (valid_symbols[TOOL_BLOCK_END] && scanner->tool_ids->size > 0) {
    return scan_tool_end(lexer, scanner);
  }

  return false;
}
