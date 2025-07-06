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

    untagged_text: $ => prec(-1, repeat1(
      seq(
        $._untagged_text_content,
        "\n"
      ))),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.web_search_tool_result,
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
      optional(seq(
        $.thinking_signature,
        '\n\n',
      )),
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

    user_header: _ => token('# USER'),

    assistant_header: _ => token('# ASSISTANT'),
    system_header: _ => token('# SYSTEM'),

    thinking_header: _ => token('# THINKING'),

    tool_use_header: _ => token('# TOOL USE'),

    tool_result_header: _ => token('# TOOL RESULT'),

    server_tool_use_header: _ => token('# SERVER TOOL USE'),

    web_search_tool_result_header: _ => token('# WEB SEARCH TOOL RESULT'),

    name: $ => seq(
      alias('Name:', $.key),
      field('value', $.value),
      /\n/
    ),

    id: $ => seq(
      alias('ID:', $.key),
      field('value', $.value),
      /\n/
    ),

    thinking_signature: $ => seq(
      alias('Signature: ', $.key),
      field('value', $.value),
    ),

    tool_param: $ => seq(
      $.tool_param_header,
      /\n/,
      optional(/\n/),
      alias($._tool_element, $.value),
    ),

    tool_param_header: $ => seq(
      '## ',
      alias($.param_name, $.name),
    ),

    param_name: $ => /[^\n]+/,

    citation_entry: $ => seq(
      alias(token(/## https?:\/\/[^\n\s]+/), $.url),
      /\n/,
      optional(/\n/),
      seq(
        alias($.citation_title, $.title),
        alias($.citation_text, $.cited_text),
        alias($.citation_encrypted_index, $.encrypted_index),
      ),
      optional(alias($.citation_text, $.cited_text)),
      optional(alias($.citation_encrypted_index, $.encrypted_index)),
    ),



    citation_title: $ => seq(
      alias('Title: ', $.key),
      optional(field("value", $.value)),
      /\n/,
    ),

    citation_text: $ => seq(
      alias('Cited text: ', $.key),
      optional(field("value", $.value)),
      /\n/,
    ),

    citation_encrypted_index: $ => seq(
      alias('Encrypted index: ', $.key),
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
      $.eval,
      $.code_block,
      $.inline_code,
      $.html_comment,
      $.safe_shell_commands,
      $.text,
    )),

    content_blocks: $ => repeat1(choice(
      $.eval,
      $.code_block,
      $.inline_code,
      $.html_comment,
      $.text,
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
      optional($.code_block_language),
      /\n/,
      optional($.code_block_content),
      '```',
    ),

    code_block_language: _ => /[^\n]*/,

    code_block_content: _ => repeat1(choice(
      /[^`\n]+/,
      /\n/,
      /`[^`]/,
      /``[^`]/,
    )),

    inline_code: $ => seq(
      '`',
      /[^`\n]+/,
      '`',
    ),

    safe_shell_commands: $ => seq(
      '<safe-shell-commands>',
      repeat(choice(
        $.eval,
        $.shell_command,
        /\n/,
      )),
      '</safe-shell-commands>',
    ),

    // TODO: allow `<` in safe shell commands, somehow...
    shell_command: _ => token(prec(-2, /[^<\n]+/)),

    eval: $ => prec(3, seq(
      $.eval_start_tag,
      optional($.eval_content),
      $.eval_end_tag,
    )),

    eval_start_tag: $ => token(prec(2, seq(
      '<eval',
      optional(seq(' ', /[^>]+/)),
      '>',
    ))),

    eval_end_tag: _ => token('</eval>'),



    eval_content: _ => prec.right(repeat1(choice(
      /[^<\n]+/,
      /\n/,
      /<[^e\/]/,
      /<e[^v]/,
      /<ev[^a]/,
      /<eva[^l]/,
      /<eval[^>]/,
      /<\/[^e]/,
      /<\/e[^v]/,
      /<\/ev[^a]/,
      /<\/eva[^l]/,
      /<\/eval[^>]/,
    ))),
  },
});
