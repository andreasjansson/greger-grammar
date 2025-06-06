/**
 * @file greger.el parser - Tree-sitter grammar for greger conversation format
 * @author Andreas Jansson <andreas.s.t.jansson@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "greger",

  extras: $ => [
    /[ \t]/,
  ],

  rules: {
    source_file: $ => seq(
      optional($.content),
      repeat($.section)
    ),

    section: $ => choice(
      $.user_section,
      $.system_section,
      $.assistant_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section,
    ),

    user_section: $ => seq(
      $.user_header,
      optional(alias($.content, $.section_content))
    ),

    system_section: $ => seq(
      $.system_header,
      optional(alias($.content, $.section_content))
    ),

    assistant_section: $ => seq(
      $.assistant_header,
      optional(alias($.content, $.section_content))
    ),

    thinking_section: $ => seq(
      $.thinking_header,
      optional(alias($.content, $.section_content))
    ),

    tool_use_section: $ => seq(
      $.tool_use_header,
      optional($.tool_use_content)
    ),

    tool_result_section: $ => seq(
      $.tool_result_header,
      optional($.tool_result_content)
    ),

    server_tool_use_section: $ => seq(
      $.server_tool_use_header,
      optional($.tool_use_content)
    ),

    server_tool_result_section: $ => seq(
      $.server_tool_result_header,
      optional($.tool_result_content)
    ),

    citations_section: $ => seq(
      $.citations_header,
      optional($.citations_content)
    ),

    // Headers - simple tokens
    user_header: $ => /##[ \t]*USER:[ \t]*\n/,
    system_header: $ => /##[ \t]*SYSTEM:[ \t]*\n/,
    assistant_header: $ => /##[ \t]*ASSISTANT:[ \t]*\n/,
    thinking_header: $ => /##[ \t]*THINKING:[ \t]*\n/,
    tool_use_header: $ => /##[ \t]*TOOL USE:[ \t]*\n/,
    tool_result_header: $ => /##[ \t]*TOOL RESULT:[ \t]*\n/,
    server_tool_use_header: $ => /##[ \t]*SERVER TOOL USE:[ \t]*\n/,
    server_tool_result_header: $ => /##[ \t]*SERVER TOOL RESULT:[ \t]*\n/,
    citations_header: $ => /##[ \t]*CITATIONS:[ \t]*\n/,

    // Content - anything that doesn't start a new section
    content: $ => prec(-1, repeat1(choice(
      $.line,
      $.newline
    ))),

    tool_use_content: $ => repeat1(choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.line,
      $.newline
    )),

    tool_result_content: $ => repeat1(choice(
      $.tool_result_id_line,
      $.tool_result_block,
      $.line,
      $.newline
    )),

    citations_content: $ => repeat1(choice(
      $.citation_url_line,
      $.citation_metadata_line,
      $.line,
      $.newline
    )),

    // Tool-specific patterns
    tool_name_line: $ => seq(
      "Name:",
      /[ \t]*/,
      $.identifier,
      "\n"
    ),

    tool_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      $.identifier,
      "\n"
    ),

    tool_parameter: $ => seq(
      /###[ \t]*/,
      $.identifier,
      "\n",
      "\n",
      $.tool_param_block
    ),

    tool_param_block: $ => seq(
      "<tool.",
      $.identifier,
      ">",
      "\n",
      repeat(choice($.line, $.newline)),
      "</tool.",
      $.identifier,
      ">"
    ),

    tool_result_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      $.identifier,
      "\n"
    ),

    tool_result_block: $ => seq(
      "<tool.",
      $.identifier,
      ">",
      "\n",
      repeat(choice($.line, $.newline)),
      "</tool.",
      $.identifier,
      ">"
    ),

    citation_url_line: $ => seq(
      /###[ \t]*/,
      $.url,
      "\n"
    ),

    citation_metadata_line: $ => seq(
      choice("Title:", "Cited text:", "Encrypted index:"),
      /[ \t]*/,
      /[^\n]*/,
      "\n"
    ),

    // Basic patterns
    line: $ => seq(
      prec(-2, /[^#\n][^\n]*/),  // Lines that don't start with # (which could be headers)
      "\n"
    ),

    newline: $ => "\n",

    // Basic tokens
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_.-]*/,
    url: $ => /https?:\/\/[^\s\n]+/,
  }
});
