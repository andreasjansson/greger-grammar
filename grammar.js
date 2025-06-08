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
    $.html_comment,
    /[ \t]/,
  ],

  externals: $ => [
    $.tool_content,
    $.html_comment,
  ],

  conflicts: $ => [
    [$.text_block],
  ],

  rules: {
    source_file: $ => repeat($._block),

    _block: $ => choice(
      $.user,
      $.assistant,
      $.system,
      $.thinking,
      $.tool_use,
      $.tool_result,
      $.server_tool_use,
      $.server_tool_result,
      $.citations,
      $.text_block,
    ),

    // Use aliases to shorten section names
    user: $ => alias($.user_section, 'user'),
    assistant: $ => alias($.assistant_section, 'assistant'),
    system: $ => alias($.system_section, 'system'),
    thinking: $ => alias($.thinking_section, 'thinking'),
    tool_use: $ => alias($.tool_use_section, 'tool_use'),
    tool_result: $ => alias($.tool_result_section, 'tool_result'),
    server_tool_use: $ => alias($.server_tool_use_section, 'server_tool_use'),
    server_tool_result: $ => alias($.server_tool_result_section, 'server_tool_result'),
    citations: $ => alias($.citations_section, 'citations'),

    user_section: $ => prec.right(seq(
      '##',
      'USER',
      ':',
      alias(repeat($._section_content), $.text),
    )),

    assistant_section: $ => prec.right(seq(
      '##',
      'ASSISTANT',
      ':',
      alias(repeat($._section_content), $.text),
    )),

    system_section: $ => prec.right(seq(
      '##',
      'SYSTEM',
      ':',
      alias(repeat($._section_content), $.text),
    )),

    thinking_section: $ => prec.right(seq(
      '##',
      'THINKING',
      ':',
      alias(repeat($._section_content), $.text),
    )),

    tool_use_section: $ => prec.right(seq(
      '##',
      'TOOL',
      'USE',
      ':',
      repeat(choice(
        field('name', $.tool_name),
        field('id', $.tool_id),
        field('param', $.tool_param),
        $._section_content,
      )),
    )),

    tool_result_section: $ => prec.right(seq(
      '##',
      'TOOL',
      'RESULT',
      ':',
      repeat(choice(
        field('id', $.tool_id),
        field('content', $.tool_content),
        $._section_content,
      )),
    )),

    server_tool_use_section: $ => prec.right(seq(
      '##',
      'SERVER',
      'TOOL',
      'USE',
      ':',
      repeat(choice(
        alias($.tool_name, $.name),
        alias($.tool_id, $.id),
        alias($.tool_param, $.param),
        $._section_content,
      )),
    )),

    server_tool_result_section: $ => prec.right(seq(
      '##',
      'SERVER',
      'TOOL',
      'RESULT',
      ':',
      repeat(choice(
        alias($.tool_id, $.id),
        alias($.tool_content, $.content),
        $._section_content,
      )),
    )),

    citations_section: $ => prec.right(seq(
      '##',
      'CITATIONS',
      ':',
      field('text', repeat($._citations_content)),
    )),

    _section_content: $ => prec(-1, choice(
      $.text_block,
      $.code_block,
      $.cite_tag,
      $.safe_shell_commands,
    )),

    tool_name: $ => token(prec(1, seq('Name:', /[^\n]*/, /\n/))),
    tool_id: $ => token(prec(1, seq('ID:', /[^\n]*/, /\n/))),

    tool_param: $ => seq(
      '###',
      /[ ]*/,
      alias($.param_name, $.name),
      /\n+/,
      alias($.tool_content, $.value),
    ),

    param_name: $ => /[^\n]+/,

    _citations_content: $ => choice(
      alias($.citation_entry, $.entry),
      alias($.citation_title, $.title),
      alias($.citation_text, $.cited_text),
      alias($.citation_encrypted_index, $.encrypted_index),
      prec(-1, $.text_block),
    ),

    citation_entry: $ => seq(
      '###',
      alias($.citation_url, $.url),
      /\n/,
    ),

    citation_url: $ => /[^\n]*/,

    citation_title: $ => field('title', token(seq(
      'Title:',
      /[ ]+/,
      /[^\n]*/,
      /\n/,
    ))),

    citation_text: $ => field('cited_text', token(seq(
      'Cited text:',
      /[ ]+/,
      /[^\n]*/,
      /\n/,
    ))),

    citation_encrypted_index: $ => field('encrypted_index', token(seq(
      'Encrypted index:',
      /[ ]+/,
      /[^\n]*/,
      /\n/,
    ))),

    text_block: $ => repeat1(choice(
      /[^\n#`<]+/,
      /\n/,
    )),

    code_block: $ => choice(
      $.triple_backtick_block,
      $.single_backtick_inline,
    ),

    triple_backtick_block: $ => seq(
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

    single_backtick_inline: $ => seq(
      '`',
      /[^`\n]+/,
      '`',
    ),

    cite_tag: $ => seq(
      '<cite>',
      /[^<]*/,
      '</cite>',
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
