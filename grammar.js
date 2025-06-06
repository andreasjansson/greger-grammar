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

  word: $ => $.identifier,

  conflicts: $ => [
    [$.user_section, $.system_section, $.assistant_section, $.thinking_section, $.tool_use_section, $.tool_result_section, $.server_tool_use_section, $.server_tool_result_section, $.citations_section],
  ],

  rules: {
    source_file: $ => seq(
      optional($.untagged_content),
      repeat($.section)
    ),

    // Content before any section headers is treated as user content
    untagged_content: $ => prec.left($._content_until_section),

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

    user_section: $ => prec.left(seq(
      $.user_header,
      optional($._content_until_section)
    )),

    system_section: $ => prec.left(seq(
      $.system_header,
      optional($._system_content_until_section)
    )),

    assistant_section: $ => prec.left(seq(
      $.assistant_header,
      optional($._content_until_section)
    )),

    thinking_section: $ => prec.left(seq(
      $.thinking_header,
      optional($._content_until_section)
    )),

    tool_use_section: $ => prec.left(seq(
      $.tool_use_header,
      optional($._tool_use_content)
    )),

    tool_result_section: $ => prec.left(seq(
      $.tool_result_header,
      optional($._tool_result_content)
    )),

    server_tool_use_section: $ => prec.left(seq(
      $.server_tool_use_header,
      optional($._tool_use_content)
    )),

    server_tool_result_section: $ => prec.left(seq(
      $.server_tool_result_header,
      optional($._tool_result_content)
    )),

    citations_section: $ => prec.left(seq(
      $.citations_header,
      optional($._citations_content)
    )),

    // Headers
    user_header: $ => token(seq("##", /[ \t]*/, "USER:", /[ \t]*/, "\n")),
    system_header: $ => token(seq("##", /[ \t]*/, "SYSTEM:", /[ \t]*/, "\n")),
    assistant_header: $ => token(seq("##", /[ \t]*/, "ASSISTANT:", /[ \t]*/, "\n")),
    thinking_header: $ => token(seq("##", /[ \t]*/, "THINKING:", /[ \t]*/, "\n")),
    tool_use_header: $ => token(seq("##", /[ \t]*/, "TOOL USE:", /[ \t]*/, "\n")),
    tool_result_header: $ => token(seq("##", /[ \t]*/, "TOOL RESULT:", /[ \t]*/, "\n")),
    server_tool_use_header: $ => token(seq("##", /[ \t]*/, "SERVER TOOL USE:", /[ \t]*/, "\n")),
    server_tool_result_header: $ => token(seq("##", /[ \t]*/, "SERVER TOOL RESULT:", /[ \t]*/, "\n")),
    citations_header: $ => token(seq("##", /[ \t]*/, "CITATIONS:", /[ \t]*/, "\n")),

    // Content patterns - stop at section headers
    _content_until_section: $ => repeat1(choice(
      $._content_line,
      $.code_block,
      $.include_tag,
      $.html_comment,
      $.inline_code,
      /\n/
    )),

    _system_content_until_section: $ => repeat1(choice(
      $.safe_shell_commands,
      $._content_line,
      $.code_block,
      $.include_tag,
      $.html_comment,
      $.inline_code,
      /\n/
    )),

    _tool_use_content: $ => repeat1(choice(
      $.tool_name,
      $.tool_id,
      $.tool_parameter,
      $._content_line,
      /\n/
    )),

    _tool_result_content: $ => repeat1(choice(
      $.tool_result_id,
      $.tool_result_value,
      $._content_line,
      /\n/
    )),

    _citations_content: $ => repeat1(choice(
      $.citation_entry,
      $._content_line,
      /\n/
    )),

    // Tool use components
    tool_name: $ => seq(
      "\n",
      "Name:",
      /[ \t]*/,
      field("name", $.identifier),
      "\n"
    ),

    tool_id: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", $.identifier),
      "\n"
    ),

    tool_parameter: $ => seq(
      "\n",
      "###",
      /[ \t]*/,
      field("param_name", $.identifier),
      "\n",
      "\n",
      field("param_value", $.tool_param_block)
    ),

    tool_param_block: $ => seq(
      "<tool.",
      field("tool_id", $.identifier),
      ">",
      "\n",
      field("content", repeat(choice(/[^<\n]+/, "\n"))),
      "</tool.",
      field("tool_id", $.identifier),
      ">"
    ),

    // Tool result components
    tool_result_id: $ => seq(
      "\n",
      "ID:",
      /[ \t]*/,
      field("id", $.identifier),
      "\n"
    ),

    tool_result_value: $ => seq(
      "\n",
      "<tool.",
      field("tool_id", $.identifier),
      ">",
      "\n",
      field("content", repeat(choice(/[^<\n]+/, "\n"))),
      "</tool.",
      field("tool_id", $.identifier),
      ">"
    ),

    // Citation components
    citation_entry: $ => seq(
      "\n",
      "###",
      /[ \t]*/,
      field("url", $.url),
      "\n",
      "\n",
      repeat($.citation_metadata)
    ),

    citation_metadata: $ => choice(
      seq("Title:", /[ \t]*/, $.text_line),
      seq("Cited text:", /[ \t]*/, $.text_line),
      seq("Encrypted index:", /[ \t]*/, $.text_line)
    ),

    // System content
    safe_shell_commands: $ => seq(
      "\n",
      "<safe-shell-commands>",
      "\n",
      field("commands", repeat($.text_line)),
      "</safe-shell-commands>"
    ),

    // Content handling - simple approach
    code_block: $ => seq(
      "```",
      optional($.language),
      "\n",
      repeat(choice(/[^`\n]+/, "\n")),
      "```"
    ),

    inline_code: $ => seq(
      "`",
      repeat(/[^`\n]+/),
      "`"
    ),

    include_tag: $ => seq(
      "<include",
      optional(/[ \t]+code/),
      ">",
      field("path", /[^<>\n]+/),
      "</include>"
    ),

    html_comment: $ => seq(
      "<!--",
      repeat(choice(/[^-\n]+/, "-")),
      "-->"
    ),

    // Basic content types
    _content_line: $ => prec(-1, seq(/[^\n]*/, "\n")),
    text_line: $ => seq(/[^\n]*/, "\n"),

    // Basic tokens
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_.-]*/,
    language: $ => /[a-zA-Z]+/,
    url: $ => /https?:\/\/[^\s\n]+/,
  }
});
