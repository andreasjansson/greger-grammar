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

  externals: $ => [
    $.tool_block_start,
    $.tool_block_end,
    $.tool_block_content,
    $._line_content,
    $._newline,
    $._citations_header,
  ],

  rules: {
    document: $ => repeat($._block),

    _block: $ => choice(
      $.regular_section,
      $.citations_with_text,
      $.citations_without_text,
      $.untagged_content,
    ),

    // Regular sections (USER, ASSISTANT, etc.) that don't contain citations
    regular_section: $ => choice(
      $.user_section,
      $.system_section,
      $.assistant_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
    ),

    // Citations with preceding text (when <cite> tag is found)
    citations_with_text: $ => seq(
      choice(
        $.assistant_section_with_cite,
        $.thinking_section_with_cite,
      ),
      $.citations_content
    ),

    // Standalone citations (when ## CITATIONS: found without preceding <cite>)
    citations_without_text: $ => seq(
      $._citations_header,
      $._newline,
      optional($.citations_content)
    ),

    // Section headers
    user_section: $ => seq(
      /##[ \t]*USER:[ \t]*/,
      $._newline,
      repeat($._content_line)
    ),

    system_section: $ => seq(
      /##[ \t]*SYSTEM:[ \t]*/,
      $._newline,
      repeat($._content_line)
    ),

    assistant_section: $ => seq(
      /##[ \t]*ASSISTANT:[ \t]*/,
      $._newline,
      repeat($._content_line)
    ),

    thinking_section: $ => seq(
      /##[ \t]*THINKING:[ \t]*/,
      $._newline,
      repeat($._content_line)
    ),

    // Special sections that contain cite tags - these will be part of citations_with_text
    assistant_section_with_cite: $ => seq(
      /##[ \t]*ASSISTANT:[ \t]*/,
      $._newline,
      repeat($._content_line_before_cite),
      $.cite_line,
      repeat($._content_line_after_cite)
    ),

    thinking_section_with_cite: $ => seq(
      /##[ \t]*THINKING:[ \t]*/,
      $._newline,
      repeat($._content_line_before_cite),
      $.cite_line,
      repeat($._content_line_after_cite)
    ),

    tool_use_section: $ => seq(
      /##[ \t]*TOOL USE:[ \t]*/,
      $._newline,
      repeat(choice(
        $.tool_name_line,
        $.tool_id_line,
        $.tool_parameter,
        $._content_line,
      ))
    ),

    tool_result_section: $ => seq(
      /##[ \t]*TOOL RESULT:[ \t]*/,
      $._newline,
      repeat(choice(
        $.tool_result_id_line,
        $.tool_result_block,
        $._content_line,
      ))
    ),

    server_tool_use_section: $ => seq(
      /##[ \t]*SERVER TOOL USE:[ \t]*/,
      $._newline,
      repeat(choice(
        $.tool_name_line,
        $.tool_id_line,
        $.tool_parameter,
        $._content_line,
      ))
    ),

    server_tool_result_section: $ => seq(
      /##[ \t]*SERVER TOOL RESULT:[ \t]*/,
      $._newline,
      repeat(choice(
        $.tool_result_id_line,
        $.tool_result_block,
        $._content_line,
      ))
    ),

    // Content line types
    _content_line: $ => choice(
      $.text_line,
      $.code_block_line,
      $.empty_line,
    ),

    _content_line_before_cite: $ => choice(
      $.text_line,
      $.code_block_line,
      $.empty_line,
    ),

    _content_line_after_cite: $ => choice(
      $.text_line_after_cite,
      $.empty_line,
    ),

    text_line: $ => seq(
      $._line_content,
      $._newline
    ),

    text_line_after_cite: $ => seq(
      $._line_content,
      $._newline
    ),

    cite_line: $ => seq(
      optional($._line_content),
      field("text", $.cite_tag),
      optional($._line_content),
      $._newline
    ),

    cite_tag: $ => seq(
      "<cite>",
      /[^<]+/,
      "</cite>"
    ),

    code_block_line: $ => seq(
      "```",
      optional($._line_content),
      $._newline
    ),

    empty_line: $ => $._newline,

    // Tool-specific elements
    tool_name_line: $ => seq(
      "Name:",
      /[ \t]*/,
      field("name", $._line_content),
      $._newline
    ),

    tool_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", $._line_content),
      $._newline
    ),

    tool_parameter: $ => seq(
      "###",
      /[ \t]*/,
      field("name", $._line_content),
      $._newline,
      $._newline,
      $.tool_parameter_block
    ),

    tool_parameter_block: $ => seq(
      $.tool_block_start,
      repeat($.tool_block_content),
      optional($.tool_block_end)
    ),

    tool_result_id_line: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", $._line_content),
      $._newline
    ),

    tool_result_block: $ => seq(
      $.tool_block_start,
      repeat($.tool_block_content),
      optional($.tool_block_end)
    ),

    // Citations content
    citations_content: $ => repeat1(choice(
      $.citation_entry,
      $.empty_line,
    )),

    citation_entry: $ => seq(
      "###",
      /[ \t]*/,
      field("url", $._line_content),
      $._newline,
      $._newline,
      repeat(choice(
        $.citation_title,
        $.citation_text,
        $.citation_index,
        $.empty_line,
      ))
    ),

    citation_title: $ => seq(
      "Title:",
      /[ \t]*/,
      field("title", $._line_content),
      $._newline
    ),

    citation_text: $ => seq(
      "Cited text:",
      /[ \t]*/,
      field("text", $._line_content),
      $._newline
    ),

    citation_index: $ => seq(
      "Encrypted index:",
      /[ \t]*/,
      field("index", $._line_content),
      $._newline
    ),

    // Untagged content at the beginning
    untagged_content: $ => prec.left(repeat1($._content_line)),
  }
});
