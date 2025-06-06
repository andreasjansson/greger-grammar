module.exports = grammar({
  name: 'greger',

  extras: $ => [
    /[ \t\r]/
  ],

  externals: $ => [
    $._section_end,
    $._text_content
  ],

  rules: {
    document: $ => seq(
      optional($.untagged_content),
      repeat($.section)
    ),

    untagged_content: $ => repeat1(choice(
      $.text_line,
      $.code_block,
      $.html_comment,
      /\n/
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

    user_section: $ => seq(
      '## USER:',
      /\n/,
      optional($.section_content)
    ),

    system_section: $ => seq(
      '## SYSTEM:',
      /\n/,
      optional($.section_content)
    ),

    assistant_section: $ => seq(
      '## ASSISTANT:',
      /\n/,
      optional($.section_content)
    ),

    thinking_section: $ => seq(
      '## THINKING:',
      /\n/,
      optional($.section_content)
    ),

    citations_section: $ => seq(
      '## CITATIONS:',
      /\n/,
      optional($.section_content)
    ),

    tool_use_section: $ => seq(
      '## TOOL USE:',
      /\n/,
      optional($.tool_use_content)
    ),

    tool_result_section: $ => seq(
      '## TOOL RESULT:',
      /\n/,
      optional($.tool_result_content)
    ),

    server_tool_use_section: $ => seq(
      '## SERVER TOOL USE:',
      /\n/,
      optional($.tool_use_content)
    ),

    server_tool_result_section: $ => seq(
      '## SERVER TOOL RESULT:',
      /\n/,
      optional($.tool_result_content)
    ),

    section_content: $ => repeat1(choice(
      $.text_line,
      $.code_block,
      $.html_comment,
      $.include_tag,
      $.safe_shell_commands_tag,
      /\n/
    )),

    tool_use_content: $ => repeat1(choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.text_line,
      $.code_block,
      $.html_comment,
      /\n/
    )),

    tool_result_content: $ => repeat1(choice(
      $.tool_id_line,
      $.tool_output,
      $.text_line,
      $.code_block,
      /\n/
    )),

    tool_name_line: $ => seq(
      'Name:',
      /[ \t]+/,
      field('name', $.identifier),
      /\n/
    ),

    tool_id_line: $ => seq(
      'ID:',
      /[ \t]+/,
      field('id', $.identifier),
      /\n/
    ),

    tool_parameter: $ => seq(
      '###',
      /[ \t]+/,
      field('name', $.identifier),
      /\n/,
      optional($.parameter_value)
    ),

    parameter_value: $ => seq(
      '<tool.',
      field('id', $.identifier),
      '>',
      /\n/,
      $._text_content,
      '</tool.',
      field('id', $.identifier),
      '>',
      /\n/
    ),

    tool_output: $ => seq(
      '<tool.',
      field('id', $.identifier),
      '>',
      /\n/,
      $._text_content,
      '</tool.',
      field('id', $.identifier),
      '>',
      /\n/
    ),

    text_line: $ => seq(
      $._text_content,
      /\n/
    ),

    code_block: $ => seq(
      '```',
      optional(field('language', $.identifier)),
      /\n/,
      repeat(choice(
        $.code_line,
        /\n/
      )),
      '```',
      /\n/
    ),

    code_line: $ => seq(
      /[^\n]*/,
      /\n/
    ),

    html_comment: $ => seq(
      '<!--',
      repeat(choice(
        /[^-]/,
        /-[^-]/,
        /--[^>]/
      )),
      '-->',
      /\n/
    ),

    include_tag: $ => seq(
      '<include',
      optional(seq(/[ \t]+/, 'code')),
      '>',
      field('path', /[^<\n]+/),
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

    identifier: $ => /[^\s\n]+/
  }
});
