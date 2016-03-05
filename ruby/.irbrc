# -*- mode: ruby -*-

require 'irb/completion'

ANSI = {
  RESET: "\e[0m",
  BOLD: "\e[1m",
  UNDERLINE: "\e[4m",
  LGRAY: "\e[0;37m",
  GRAY: "\e[1;30m",
  RED: "\e[31m",
  GREEN: "\e[32m",
  YELLOW: "\e[33m",
  BLUE: "\e[34m",
  MAGENTA: "\e[35m",
  CYAN: "\e[36m",
  WHITE: "\e[37m"
}

def colorize(text, color)
  "#{ANSI[color]}#{text}#{ANSI[:RESET]}"
end

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:PROMPT][:COLOR] = {
  PROMPT_I: colorize('>> ', :GRAY),
  PROMPT_N: colorize('>> ', :GRAY),
  PROMPT_S: '',
  PROMPT_C: colorize('?> ', :GRAY),
  RETURN: colorize('=>', :GRAY) + colorize(" %s\n", :WHITE)
}
IRB.conf[:PROMPT_MODE] = :COLOR
