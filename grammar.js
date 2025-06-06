// Token definitions for complex patterns
const SECTION_HEADER = token(choice(
  '## USER:',
  '## SYSTEM:',
  '## ASSISTANT:',
  '## THINKING:',
  '## TOOL USE:',
  '## TOOL RESULT:',
  '## SERVER TOOL USE:',
  '## SERVER TOOL RESULT:',
  '## CITATIONS:'
));

const CODE_BLOCK = token(seq(
  '```',
  optional(/[^\n]*/), // language
  /\n/,
  repeat(choice(
    /[^`\n]+/,
    /`[^`]/,
    /``[^`]/,
    /\n/
  )),
  '```'
));

const HTML_COMMENT = token(seq(
  '<!--',
  repeat(choice(
    /[^-]+/,
    /-[^-]/,
    /--[^>]/
  )),
  '-->'
));

const TOOL_TAG_OPEN = token(seq('<tool.', /[^\s>]+/, '>'));
const TOOL_TAG_CLOSE = token(seq('</tool.', /[^\s>]+/, '>'));

const INCLUDE_TAG = token(seq(
  '<include',
  optional(seq(/\s+/, 'code')),
  '>',
  /[^<\n]+/,
  '</include>'
));

const SAFE_SHELL_COMMANDS_OPEN = token('<safe-shell-commands>');
const SAFE_SHELL_COMMANDS_CLOSE = token('</safe-shell-commands>');

const CITE_OPEN = token('<cite>');
const CITE_CLOSE = token('</cite>');

const TOOL_NAME_LINE = token(seq('Name:', /\s+/, /[^\s\n]+/));
const TOOL_ID_LINE = token(seq('ID:', /\s+/, /[^\s\n]+/));
const TOOL_PARAMETER_HEADER = token(seq('###', /\s+/, /[^\s\n]+/));

module.exports = grammar({
  name: 'greger',

  extras: $ => [
    /\s/
  ],

  rules: {
    document: $ => repeat(choice(
      $.section,
      $.content_block,
      $.untagged_text
    )),

    section: $ => seq(
      field('header', $.section_header),
      optional(field('content', $.section_content))
    ),

    section_header: $ => choice(
      $.user_header,
      $.system_header,
      $.assistant_header,
      $.thinking_header,
      $.tool_use_header,
      $.tool_result_header,
      $.server_tool_use_header,
      $.server_tool_result_header,
      $.citations_header
    ),

    user_header: $ => '## USER:',
    system_header: $ => '## SYSTEM:',
    assistant_header: $ => '## ASSISTANT:',
    thinking_header: $ => '## THINKING:',
    tool_use_header: $ => '## TOOL USE:',
    tool_result_header: $ => '## TOOL RESULT:',
    server_tool_use_header: $ => '## SERVER TOOL USE:',
    server_tool_result_header: $ => '## SERVER TOOL RESULT:',
    citations_header: $ => '## CITATIONS:',

    section_content: $ => repeat1(choice(
      $.content_block,
      $.tool_element,
      $.text_line
    )),

    content_block: $ => choice(
      $.code_block,
      $.html_comment,
      $.include_tag,
      $.safe_shell_commands_block,
      $.cite_block
    ),

    tool_element: $ => choice(
      $.tool_name_line,
      $.tool_id_line,
      $.tool_parameter,
      $.tool_output_block
    ),

    code_block: $ => CODE_BLOCK,
    html_comment: $ => HTML_COMMENT,
    include_tag: $ => INCLUDE_TAG,

    safe_shell_commands_block: $ => seq(
      SAFE_SHELL_COMMANDS_OPEN,
      repeat($.text_line),
      SAFE_SHELL_COMMANDS_CLOSE
    ),

    cite_block: $ => seq(
      CITE_OPEN,
      repeat(choice(
        $.text_content,
        $.inline_code
      )),
      CITE_CLOSE
    ),

    tool_name_line: $ => TOOL_NAME_LINE,
    tool_id_line: $ => TOOL_ID_LINE,

    tool_parameter: $ => seq(
      field('header', $.tool_parameter_header),
      optional(field('value', $.tool_parameter_value))
    ),

    tool_parameter_header: $ => TOOL_PARAMETER_HEADER,

    tool_parameter_value: $ => seq(
      $.tool_tag_open,
      repeat(choice(
        $.text_line,
        $.code_block
      )),
      $.tool_tag_close
    ),

    tool_output_block: $ => seq(
      $.tool_tag_open,
      repeat(choice(
        $.text_line,
        $.code_block
      )),
      $.tool_tag_close
    ),

    tool_tag_open: $ => TOOL_TAG_OPEN,
    tool_tag_close: $ => TOOL_TAG_CLOSE,

    inline_code: $ => choice(
      seq('`', /[^`\n]*/, '`'),
      seq('``', /[^`\n]*/, '``')
    ),

    text_line: $ => seq(
      optional($.text_content),
      /\n/
    ),

    untagged_text: $ => $.text_line,

    text_content: $ => /[^\n]+/
  }
});
