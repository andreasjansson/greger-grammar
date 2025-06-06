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
    // Don't treat newlines as whitespace since they're significant
    /[ \t]/,
  ],

  rules: {
    source_file: $ => seq(
      optional($.untagged_content),
      repeat($.section)
    ),

    // Content before any section headers is treated as user content
    untagged_content: $ => $._content,

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
      optional($._content)
    ),

    system_section: $ => seq(
      $.system_header,
      optional($._system_content)
    ),

    assistant_section: $ => seq(
      $.assistant_header,
      optional($._content)
    ),

    thinking_section: $ => seq(
      $.thinking_header,
      optional($._content)
    ),

    tool_use_section: $ => seq(
      $.tool_use_header,
      optional($.tool_name),
      optional($.tool_id),
      repeat($.tool_parameter)
    ),

    tool_result_section: $ => seq(
      $.tool_result_header,
      optional($.tool_result_id),
      optional($.tool_result_content)
    ),

    server_tool_use_section: $ => seq(
      $.server_tool_use_header,
      optional($.tool_name),
      optional($.tool_id),
      repeat($.tool_parameter)
    ),

    server_tool_result_section: $ => seq(
      $.server_tool_result_header,
      optional($.tool_result_id),
      optional($.tool_result_content)
    ),

    citations_section: $ => seq(
      $.citations_header,
      repeat($.citation_entry)
    ),

    // Headers
    user_header: $ => seq("##", /[ \t]*/, "USER:", $._newline),
    system_header: $ => seq("##", /[ \t]*/, "SYSTEM:", $._newline),
    assistant_header: $ => seq("##", /[ \t]*/, "ASSISTANT:", $._newline),
    thinking_header: $ => seq("##", /[ \t]*/, "THINKING:", $._newline),
    tool_use_header: $ => seq("##", /[ \t]*/, "TOOL USE:", $._newline),
    tool_result_header: $ => seq("##", /[ \t]*/, "TOOL RESULT:", $._newline),
    server_tool_use_header: $ => seq("##", /[ \t]*/, "SERVER TOOL USE:", $._newline),
    server_tool_result_header: $ => seq("##", /[ \t]*/, "SERVER TOOL RESULT:", $._newline),
    citations_header: $ => seq("##", /[ \t]*/, "CITATIONS:", $._newline),

    // Tool use components
    tool_name: $ => seq(
      $._newline,
      "Name:",
      /[ \t]*/,
      field("name", $.identifier),
      $._newline
    ),

    tool_id: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", $.identifier),
      $._newline
    ),

    tool_parameter: $ => seq(
      $._newline,
      "###",
      /[ \t]*/,
      field("param_name", $.identifier),
      $._newline,
      $._newline,
      field("param_value", $.tool_param_content)
    ),

    tool_param_content: $ => seq(
      $.tool_tag_open,
      $._newline,
      field("content", optional($._tool_content)),
      $._newline,
      $.tool_tag_close
    ),

    tool_tag_open: $ => seq("<tool.", $.identifier, ">"),
    tool_tag_close: $ => seq("</tool.", $.identifier, ">"),

    // Tool result components
    tool_result_id: $ => seq(
      $._newline,
      "ID:",
      /[ \t]*/,
      field("id", $.identifier),
      $._newline
    ),

    tool_result_content: $ => seq(
      $._newline,
      $.tool_tag_open,
      $._newline,
      field("content", optional($._tool_content)),
      $._newline,
      $.tool_tag_close
    ),

    // Citation components
    citation_entry: $ => seq(
      $._newline,
      "###",
      /[ \t]*/,
      field("url", $.url),
      $._newline,
      $._newline,
      repeat($.citation_metadata)
    ),

    citation_metadata: $ => choice(
      seq("Title:", /[ \t]*/, $._line_content, $._newline),
      seq("Cited text:", /[ \t]*/, $._line_content, $._newline),
      seq("Encrypted index:", /[ \t]*/, $._line_content, $._newline)
    ),

    // System content (can contain safe-shell-commands)
    _system_content: $ => repeat1(choice(
      $.safe_shell_commands,
      $.include_tag,
      $._content_line,
      $._newline
    )),

    safe_shell_commands: $ => seq(
      $._newline,
      "<safe-shell-commands>",
      $._newline,
      field("commands", repeat(seq($._line_content, $._newline))),
      "</safe-shell-commands>"
    ),

    // Content handling - need to be careful about code blocks
    _content: $ => repeat1(choice(
      $.code_block,
      $.inline_code,
      $.include_tag,
      $.html_comment,
      $._content_line,
      $._newline
    )),

    _tool_content: $ => repeat1(choice(
      $._tool_content_line,
      $._newline
    )),

    code_block: $ => choice(
      $.triple_backtick_block,
      $.double_backtick_block
    ),

    triple_backtick_block: $ => seq(
      "```",
      optional($.language),
      $._newline,
      repeat(choice($._code_line, $._newline)),
      "```"
    ),

    double_backtick_block: $ => seq(
      "``",
      repeat(choice($._code_line_inline, /[^`]/)),
      "``"
    ),

    inline_code: $ => seq(
      "`",
      repeat(choice($._code_line_inline, /[^`]/)),
      "`"
    ),

    include_tag: $ => seq(
      "<include",
      optional("code"),
      ">",
      field("path", repeat1(/[^<>/\n]+/)),
      "</include>"
    ),

    html_comment: $ => seq(
      "<!--",
      repeat(choice(/[^-]/, seq("-", /[^-]/))),
      "-->"
    ),

    // Basic content types
    _content_line: $ => seq($._line_content, $._newline),
    _tool_content_line: $ => seq($._tool_line_content, $._newline),
    _code_line: $ => seq($._line_content, $._newline),
    _code_line_inline: $ => /[^\n`]+/,

    _line_content: $ => repeat1(choice(
      /[^\n#<]/,
      seq("#", /[^#\n]/),
      seq("##", /[^A-Z \t\n]/),
      seq("<", /[^s!/\n]/),
      seq("<s", /[^a\n]/),
      seq("<sa", /[^f\n]/),
      seq("<saf", /[^e\n]/),
      seq("<safe", /[-a-z]/),
      seq("<!", /[^-\n]/),
      seq("<!-", /[^-\n]/)
    )),

    _tool_line_content: $ => repeat1(choice(
      /[^\n<]/,
      seq("<", /[^\/\n]/),
      seq("<\/", /[^t\n]/),
      seq("<\/t", /[^o\n]/),
      seq("<\/to", /[^o\n]/),
      seq("<\/too", /[^l\n]/),
      seq("<\/tool", /[^.\n]/)
    )),

    // Basic tokens
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_.-]*/,
    language: $ => /[a-zA-Z]+/,
    url: $ => /https?:\/\/[^\s\n]+/,
    _newline: $ => /\n/,
  }
});
