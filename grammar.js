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
    $.inline_code,
    $.code_fence_start,
    $.code_fence_end,
    $.code_fence_content,
    $._newline,
    $._eof,
  ],

  rules: {
    document: $ => repeat($._block),

    _block: $ => choice(
      $.section,
      $.untagged_content,
    ),

    // Different section types
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

    // Section headers
    user_section: $ => seq(
      alias(/##[ \t]*USER:[ \t]*/, $.section_header),
      $._newline,
      optional($.section_content)
    ),

    system_section: $ => seq(
      alias(/##[ \t]*SYSTEM:[ \t]*/, $.section_header),
      $._newline,
      optional($.section_content)
    ),

    assistant_section: $ => seq(
      alias(/##[ \t]*ASSISTANT:[ \t]*/, $.section_header),
      $._newline,
      optional($.section_content)
    ),

    thinking_section: $ => seq(
      alias(/##[ \t]*THINKING:[ \t]*/, $.section_header),
      $._newline,
      optional($.section_content)
    ),

    tool_use_section: $ => seq(
      alias(/##[ \t]*TOOL USE:[ \t]*/, $.section_header),
      $._newline,
      optional($.tool_use_content)
    ),

    tool_result_section: $ => seq(
      alias(/##[ \t]*TOOL RESULT:[ \t]*/, $.section_header),
      $._newline,
      optional($.tool_result_content)
    ),

    server_tool_use_section: $ => seq(
      alias(/##[ \t]*SERVER TOOL USE:[ \t]*/, $.section_header),
      $._newline,
      optional($.tool_use_content)
    ),

    server_tool_result_section: $ => seq(
      alias(/##[ \t]*SERVER TOOL RESULT:[ \t]*/, $.section_header),
      $._newline,
      optional($.tool_result_content)
    ),

    citations_section: $ => seq(
      alias(/##[ \t]*CITATIONS:[ \t]*/, $.section_header),
      $._newline,
      optional($.citations_content)
    ),

    // Content within sections
    section_content: $ => repeat1($._content_element),

    _content_element: $ => choice(
      $.text,
      $.cite_tag,
      $.code_block,
      $.inline_code,
      $._newline,
    ),

    // Text content - anything that's not markup
    text: $ => /[^#<`\n]+/,

    // Citation tags
    cite_tag: $ => seq(
      "<cite>",
      field("text", repeat1(/[^<]+/)),
      "</cite>"
    ),

    // Code blocks
    code_block: $ => seq(
      $.code_fence_start,
      optional($.code_fence_content),
      optional($.code_fence_end)
    ),

    // Tool use content
    tool_use_content: $ => repeat1(choice(
      $.tool_name,
      $.tool_id,
      $.tool_parameter,
      $._newline,
    )),

    tool_name: $ => seq(
      "Name:",
      /[ \t]*/,
      field("name", /[^\n]+/),
      $._newline
    ),

    tool_id: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", /[^\n]+/),
      $._newline
    ),

    tool_parameter: $ => seq(
      "###",
      /[ \t]*/,
      field("name", /[^\n]+/),
      $._newline,
      $._newline,
      $.tool_parameter_block
    ),

    tool_parameter_block: $ => seq(
      $.tool_block_start,
      optional(repeat($.tool_block_content)),
      optional($.tool_block_end)
    ),

    // Tool result content
    tool_result_content: $ => repeat1(choice(
      $.tool_result_id,
      $.tool_result_block,
      $._newline,
    )),

    tool_result_id: $ => seq(
      "ID:",
      /[ \t]*/,
      field("id", /[^\n]+/),
      $._newline
    ),

    tool_result_block: $ => seq(
      $.tool_block_start,
      optional(repeat($.tool_block_content)),
      optional($.tool_block_end)
    ),

    // Citations content
    citations_content: $ => repeat1(choice(
      $.citation_entry,
      $._newline,
    )),

    citation_entry: $ => seq(
      "###",
      /[ \t]*/,
      field("url", /[^\n]+/),
      $._newline,
      $._newline,
      repeat(choice(
        $.citation_title,
        $.citation_text,
        $.citation_index,
        $._newline,
      ))
    ),

    citation_title: $ => seq(
      "Title:",
      /[ \t]*/,
      field("title", /[^\n]+/),
      $._newline
    ),

    citation_text: $ => seq(
      "Cited text:",
      /[ \t]*/,
      field("text", /[^\n]+/),
      $._newline
    ),

    citation_index: $ => seq(
      "Encrypted index:",
      /[ \t]*/,
      field("index", /[^\n]+/),
      $._newline
    ),

    // Untagged content at the beginning of the document
    untagged_content: $ => seq(
      $.text,
      repeat($._content_element)
    ),
  }
});
