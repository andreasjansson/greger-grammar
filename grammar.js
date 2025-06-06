module.exports = grammar({
  name: 'greger',

  extras: $ => [
    /[ \t]/
  ],



  rules: {
    document: $ => repeat(choice(
      $.section,
      $.untagged_content
    )),

    section: $ => choice(
      $.user_section,
      $.system_section,
      $.assistant_section,
      $.thinking_section,
      $.tool_use_section,
      $.tool_result_section,
      $.server_tool_use_section,
      $.server_tool_result_section,
      $.citations_section
    ),

    // Basic sections
    user_section: $ => prec.left(seq(
      '## USER:',
      /\n/,
      repeat($.section_item)
    )),

    system_section: $ => prec.left(seq(
      '## SYSTEM:',
      /\n/,
      repeat($.section_item)
    )),

    assistant_section: $ => prec.left(seq(
      '## ASSISTANT:',
      /\n/,
      repeat($.section_item)
    )),

    thinking_section: $ => prec.left(seq(
      '## THINKING:',
      /\n/,
      repeat($.section_item)
    )),

    citations_section: $ => prec.left(seq(
      '## CITATIONS:',
      /\n/,
      repeat($.section_item)
    )),

    // Tool sections
    tool_use_section: $ => prec.left(seq(
      '## TOOL USE:',
      /\n/,
      repeat($.tool_use_item)
    )),

    tool_result_section: $ => prec.left(seq(
      '## TOOL RESULT:',
      /\n/,
      repeat($.tool_result_item)
    )),

    server_tool_use_section: $ => prec.left(seq(
      '## SERVER TOOL USE:',
      /\n/,
      repeat($.tool_use_item)
    )),

    server_tool_result_section: $ => prec.left(seq(
      '## SERVER TOOL RESULT:',
      /\n/,
      repeat($.tool_result_item)
    )),

    // Section items
    section_item: $ => choice(
      $.text_line,
      $.code_block,
      $.html_comment,
      $.include_tag,
      $.safe_shell_commands_tag,
      /\n/
    ),

    tool_use_item: $ => choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.text_line,
      /\n/
    ),

    tool_result_item: $ => choice(
      $.tool_id_line,
      $.tool_output,
      $.text_line,
      /\n/
    ),

    // Tool elements
    tool_name_line: $ => seq(
      'Name:',
      /[ \t]/,
      $.identifier,
      /\n/
    ),

    tool_id_line: $ => seq(
      'ID:',
      /[ \t]/,
      $.identifier,
      /\n/
    ),

    tool_parameter: $ => seq(
      '###',
      /[ \t]/,
      $.identifier,
      /\n/,
      optional($.parameter_value)
    ),

    parameter_value: $ => seq(
      '<tool.',
      $.identifier,
      '>',
      /\n/,
      repeat(choice(
        $.text_line,
        $.code_block,
        /\n/
      )),
      '</tool.',
      $.identifier,
      '>',
      /\n/
    ),

    tool_output: $ => seq(
      '<tool.',
      $.identifier,
      '>',
      /\n/,
      repeat(choice(
        $.text_line,
        $.code_block,
        /\n/
      )),
      '</tool.',
      $.identifier,
      '>',
      /\n/
    ),

    // Code blocks
    code_block: $ => seq(
      '```',
      optional($.identifier),
      /\n/,
      repeat(choice(
        $.text_line,
        /\n/
      )),
      '```',
      /\n/
    ),

    // Comments and tags
    html_comment: $ => seq(
      '<!--',
      repeat(/[^-]/),
      '-->',
      /\n/
    ),

    include_tag: $ => seq(
      '<include',
      optional(seq(/[ \t]+/, 'code')),
      '>',
      /[^<\n]+/,
      '</include>',
      /\n/
    ),

    safe_shell_commands_tag: $ => seq(
      '<safe-shell-commands>',
      /\n/,
      repeat(seq(
        /[^\n<]+/,
        /\n/
      )),
      '</safe-shell-commands>',
      /\n/
    ),

    // Basic content
    text_line: $ => seq(
      /[^\n]+/,
      /\n/
    ),

    untagged_content: $ => choice(
      $.text_line,
      /\n/
    ),

    identifier: $ => /[^\s\n]+/
  }
});
