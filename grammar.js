/**
 * @file Greger grammar for tree-sitter
 * @author Andreas Jansson
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "greger",

  extras: $ => [
    /[ \t\n]/,
  ],

  externals: $ => [
    $.tool_start_tag,
    $.tool_end_tag,
    $.tool_content_head,
    $.tool_content_tail,
    $.html_comment,
  ],

  inline: $ => [
    $.content_blocks,
    $.assistant_content_blocks,
    $.system_content_blocks,
  ],



  rules: {

    source_file: $ => seq(
      optional($.untagged_text),
      optional($.system),
      repeat($._block),
    ),

    untagged_text: $ => prec(-1,
      repeat1(seq(
        $._untagged_text_content,
        "\n"
      ))
    ),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.web_search_tool_result,
      $.citations,
    ),

    user: $ => seq(
      $.user_header,
      '\n\n',
      $.content_blocks,
    ),

    assistant: $ => seq(
      $.assistant_header,
      '\n\n',
      $.assistant_content_blocks,
    ),

    system: $ => seq(
      $.system_header,
      '\n\n',
      $.system_content_blocks,
    ),

    thinking: $ => seq(
      $.thinking_header,
      '\n\n',
      $.content_blocks,
    ),

    tool_use: $ => seq(
      $.tool_use_header,
      '\n\n',
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    tool_result: $ => seq(
      $.tool_result_header,
      '\n\n',
      $.id,
      $.content,
    ),

    server_tool_use: $ => seq(
      $.server_tool_use_header,
      '\n\n',
      repeat(choice(
        $.name,
        $.id,
        $.tool_param,
      )),
    ),

    web_search_tool_result: $ => seq(
      $.web_search_tool_result_header,
      '\n\n',
      $.id,
      $.content,
    ),

    citations: $ => seq(
      $.citations_header,
      '\n\n',
      repeat($.citation_entry),
    ),

    user_header: _ => token('## USER:'),

    assistant_header: _ => token('## ASSISTANT:'),
    system_header: _ => token('## SYSTEM:'),

    thinking_header: _ => token('## THINKING:'),

    tool_use_header: _ => token('## TOOL USE:'),

    tool_result_header: _ => token('## TOOL RESULT:'),

    server_tool_use_header: _ => token('## SERVER TOOL USE:'),

    web_search_tool_result_header: _ => token('## WEB SEARCH TOOL RESULT:'),

    citations_header: _ => token('## CITATIONS:'),

    name: $ => seq(
      'Name:',
      field('value', $.value),
      /\n/
    ),

    id: $ => seq(
      'ID:',
      field('value', $.value),
      /\n/
    ),

    tool_param: $ => seq(
      '### ',
      alias($.param_name, $.name),
      /\n/,
      optional(/\n/),
      alias($._tool_element, $.value),
    ),

    param_name: $ => /[^\n]+/,

    citation_entry: $ => seq(
      alias(token(/### https?:\/\/[^\n\s]+/), $.url),
      /\n/,
      optional(/\n/),
      choice(
        alias($.citation_title, $.title),
        alias($.citation_text, $.cited_text),
        alias($.citation_encrypted_index, $.encrypted_index),
      ),
      optional(alias($.citation_text, $.cited_text)),
      optional(alias($.citation_encrypted_index, $.encrypted_index)),
    ),



    citation_title: $ => seq(
      'Title: ',
      field("value", $.value),
      /\n/,
    ),

    citation_text: $ => seq(
      'Cited text: ',
      field("value", $.value),
      /\n/,
    ),

    citation_encrypted_index: $ => seq(
      'Encrypted index: ',
      field("value", $.value),
      /\n/,
    ),

    value: _ => /[^\n]+/,

    assistant_content_blocks: $ => repeat1(choice(
      $.citation_entry,
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
    )),

    system_content_blocks: $ => repeat1(choice(
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
      $.safe_shell_commands,
    )),

    content_blocks: $ => repeat1(choice(
      $.text,
      $.code_block,
      $.inline_code,
      $.html_comment,
    )),

    text: $ => prec.right(repeat1(choice(
      $._text_content,
      /\n/,
    ))),

    _text_content: $ => token(prec(-1, /[^`\n]+/)),

    _untagged_text_content: $ => token(prec(-2, seq(/[^#\n]+/, '\n'))),

    _tool_element: $ => seq(
      $.tool_start_tag,
      field('value', $.tool_content),
      $.tool_end_tag,
    ),

    tool_content: $ => seq(
      $.tool_content_head,
      optional($.tool_content_tail),
    ),

    content: $ => alias($._tool_element, 'content'),

    code_block: $ => seq(
      '```',
      optional(/[^\n]*/),
      /\n/,
      repeat(choice(
        /[^`\n]+/,
        /\n/,
        /`[^`]/,
        /``[^`]/,
      )),
      '```',
    ),

    inline_code: $ => seq(
      '`',
      /[^`\n]+/,
      '`',
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(choice(
        /[^<\n]+/,
        /\n/,
      )),
      '</safe-shell-commands>',
    ),
  }
});
