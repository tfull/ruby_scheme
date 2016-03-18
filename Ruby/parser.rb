# -*- coding: utf-8 -*-

module Parser
  PECULIAR_SYMBOLS = ["!","$","%","&","*","+","-",".","/",":","<","=",">","?","@","^","_","~"]
  LEFT_PARS = ["(","[","{"]
  RIGHT_PARS = [")","]","}"]

  REGULAR_EXPRESSION_INTEGER = /^(?:[-+])?\d+$/
  REGULAR_EXPRESSION_FLOAT = /^(?:[-+])?(?:\d+\.\d*|\d*\.\d+)$/
  REGULAR_EXPRESSION_RATIONAL = /^(?:[-+])?\d+\/\d+$/
  REGULAR_EXPRESSION_COMPLEX = /^(?:(?:[-+])?(?:\d+|\d+\.\d*|\d*\.\d+|\d+\/\d+))?[-+](?:\d+|\d+\.\d*|\d*\.\d+|\d+\/\d+)?i$/

  def self.drop_token_or_number(s)
    if s =~ REGULAR_EXPRESSION_INTEGER
      Type::Token.new(:integer, s.to_i)
    elsif s =~ REGULAR_EXPRESSION_FLOAT
      Type::Token.new(:float, s.to_f)
    elsif s =~ REGULAR_EXPRESSION_RATIONAL
      Type::Token.new(:rational, Rational(s))
    elsif s =~ REGULAR_EXPRESSION_COMPLEX
      Type::Token.new(:complex, Complex(s))
    elsif s == "."
      Type::Token.new(:period)
    else
      Type::Token.new(:token, s.to_sym)
    end
  end

  def self.drop_character(s)
    if s.length == 1
      s.to_sym
    elsif s == "newline"
      :"\n"
    elsif s == "space"
      :" "
    elsif s == "tab"
      :"\t"
    else
      raise Type::LexError, "no such character #{s}"
    end
  end

  def self.normal_symbol?(c)
    c.length == 1 && (c =~ /[a-zA-Z0-9]/ || PECULIAR_SYMBOLS.member?(c))
  end

  def self.lex(info)   # status = { :mode, :tokens, :store, :input }
    while (s = info[:input].shift)
      case info[:mode]
      when :plain, :quote, :character_newline then
        if info[:mode] == :character_newline
          info[:tokens].push(Type::Token.new(:character, drop_character("\n")))
        end
        case s
        when "\n","\t"," " then
          if info[:mode] == :character_newline
            info[:mode] = :plain
          end
        when "(","[","{" then
          info[:tokens].push(Type::Token.new(:left, LEFT_PARS.find_index(s)))
          info[:mode] = :plain
        when ")","]","}" then 
          info[:tokens].push(Type::Token.new(:right, RIGHT_PARS.find_index(s)))
          info[:mode] = :plain
        when "#" then
          info[:mode] = :sharp
        when "\\" then
          info[:mode] = :token
          info[:store] = s
        when "\"" then
          info[:mode] = :string
        when "`" then
          info[:tokens].push(Type::Token.new(:quasiquote))
          info[:mode] = :quote
        when "'" then
          info[:tokens].push(Type::Token.new(:quote))
          info[:mode] = :quote
        when "," then
          info[:mode] = :comma
        when ";" then
          info[:mode] = :comment
        else 
          if normal_symbol?(s)
            info[:mode] = :token
            info[:store] = s
          else
            raise Type::LexError, "unexpected #{s} in input"
          end
        end
      when :token then
        case s
        when "\n"," ","\t" then
          info[:mode] = :plain
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:store] = ""
        when "(","[","{" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:tokens].push(Type::Token.new(:left, LEFT_PARS.find_index(s)))
          info[:mode] = :plain
          info[:store] = ""
        when ")","]","}" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:tokens].push(Type::Token.new(:right, RIGHT_PARS.find_index(s)))
          info[:mode] = :plain
          info[:store] = ""
        when "#" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:mode] = :sharp
          info[:store] = ""
        when "\\" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:mode] = :back_slash
          info[:store] = ""
        when "\"" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:mode] = :string
          info[:store] = ""
        when ";" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:mode] = :comment
          info[:store] = ""
        when "`" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:tokens].push(Type::Token.new(:quasiquote))
          info[:mode] = :quote
          info[:store] = ""
        when "'" then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:tokens].push(Type::Token.new(:quasiquote))
          info[:mode] = :quote
          info[:store] = ""
        when "," then
          info[:tokens].push(drop_token_or_number(info[:store]))
          info[:store] = ""
          info[:mode] = :comma
        else
          if normal_symbol?(s)
            info[:store] += s
          else
            raise Type::LexError, "unexpected #{s} in token"
          end
        end
      when :sharp then
        case s
        when "t" then
          info[:tokens].push(Type::Token.new(:boolean, true))
          info[:mode] = :plain
        when "f" then
          info[:tokens].push(Type::Token.new(:boolean, false))
          info[:mode] = :plain
        when "\\" then
          info[:mode] = :character
        when "(" then
          info[:tokens].push(Type::Token.new(:vector_start))
          info[:mode] = :plain
        else
          raise Type::LexError, "unexpected #{s} in sharp"
        end
      when :character then
        case s
        when "(","[","{" then
          if info[:store].length == 0
            info[:tokens].push(Type::Token.new(:character, drop_character(s)))
            info[:mode] = :plain
          else
            info[:tokens].push(Type::Token.new(:character, drop_character(info[:store])))
            info[:tokens].push(Type::Token.new(:left, LEFT_PARS.find_index(s)))
            info[:store] = ""
            info[:mode] = :plain
          end
        when ")","]","}" then
          if info[:store].length == 0
            info[:tokens].push(Type::Token.new(:character, drop_character(s)))
            info[:mode] = :plain
          else
            info[:tokens].push(Type::Token.new(:character, drop_character(info[:store])))
            info[:tokens].push(Type::Token.new(:right, RIGHT_PARS.find_index(s)))
            info[:store] = ""
            info[:mode] = :plain
          end
        when "\\" then
          if info[:store].length == 0
            info[:tokens].push(Type::Token.new(:character, drop_character(s)))
            info[:mode] = :plain
          else
            info[:tokens].push(Type::Token.new(:character, drop_character(info[:store])))
            info[:store] = "\\"
            info[:mode] = :token
          end
        when "\n"," ","\t" then
          if info[:store].length == 0
            if s == "\n"
              info[:mode] = :character_newline                 # character newline
            else
              info[:tokens].push(Type::Token.new(:character, drop_character(s)))
              info[:mode] = :plain
            end
          else
            info[:tokens].push(Type::Token.new(:character, drop_character(info[:store])))
            info[:mode] = :plain
            info[:store] = ""
          end
        when ";" then
          if info[:store].length == 0
            info[:tokens].push(Type::Token.new(:character, drop_character(s)))
            info[:mode] = :plain
          else
            info[:tokens].push(Type::Token.new(:character, drop_character(info[:store])))
            info[:store] = ""
            info[:mode] = :comment
          end
        else
          if normal_symbol?(s)
            info[:store] += s
          else
            raise Type::LexError, "unexpected #{s} in character"
          end
        end
      when :string then
        case s
        when "\"" then
          info[:tokens].push(Type::Token.new(:string, info[:store]))
          info[:store] = ""
          info[:mode] = :plain
        when "\\" then
          info[:mode] = :string_escape
        else
          info[:store] += s
        end
      when :string_escape then
        case s
        when "\n" then
          info[:mode] = :string
        when "\"","\\" then
          info[:store] += s
          info[:mode] = :string
        when "n" then
          info[:store] += "\n"
          info[:mode] = :string
        when "t" then
          info[:store] += "\t"
          info[:mode] = :string
        else
          raise Type::LexError, "unexpected #{s} in string escape"
        end
      when :comma then
        case s
        when "@" then
          info[:tokens].push(Type::Token.new(:"unquote-splicing"))
          info[:mode] = :plain
        when "\n","\t"," " then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :plain
        when "(","[","{" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:tokens].push(Type::Token.new(:left, LEFT_PARS.find_index(s)))
          info[:mode] = :plain
        when ")","]","}" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:tokens].push(Type::Token.new(:right, RIGHT_PARS.find_index(s)))
          info[:mode] = :plain
        when "#" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :sharp
        when "\\" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :token
          info[:store] = s
        when "\"" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :string
        when "`" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :quote
        when "'" then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :quote
        when "," then
          info[:tokens].push(Type::Token.new(:unquote))
          info[:mode] = :comma
        when ";" then
          info[:tokens].psuh(Type::Token.new(:unquote))
          info[:mode] = :comment
        else
          if normal_symbol?(s)
            info[:mode] = :token
            info[:tokens].push(Type::Token.new(:unquote))
            info[:store] = s
          else
            raise Type::LexError, "unexpected #{s} in comma"
          end
        end
      when :comment then
        if s == "\n"
          info[:mode] = :plain
        end
      else
        raise Type::LexError, "unexpected mode #{info[:mode]}"
      end
    end
  end
  
  def self.parse(tokens)
    raise Type::ParseError, "EoI while reading" if tokens.length == 0
    token = tokens.shift
    if token.tk_type == :left
      datas = []
      until tokens[0].tk_type == :right
        datas.push(parse(tokens))
      end
      n_token = tokens.shift
      raise Type::ParseError, "unmatched parenthesis type" unless token.tk_val == n_token.tk_val
      Type::Expression.new(:list, datas)
    elsif token.tk_type == :right
      raise Type::ParseError, "unexpected right parenthesis"
    elsif token.tk_type == :vector_start
      datas = []
      until tokens[0].tk_type == :right
        datas.push(parse(tokens))
      end
      n_token = tokens.shift
      raise Type::ParseError, "unmatched parenthesis type" unless n_token.tk_val == 0
      Type::Expression.new(:vector, datas)
    elsif token.tk_type == :quote || token.tk_type == :unquote || token.tk_type == :quasiquote || token.tk_type == :"unquote-splicing"
      Type::Expression.new(token.tk_type, parse(tokens))
    elsif token.tk_type == :period
      Type::Expression.new(token.tk_type)
    else
      Type::Expression.new(token.tk_type, token.tk_val)
    end
  end

  def self.parsable?(tokens, iter = 0)
    return false if tokens.length < iter + 1
    token = tokens[iter]
    if token.tk_type == :left
      iter += 1
      return false if tokens[iter] == nil
      until tokens[iter].tk_type == :right
        res = parsable?(tokens, iter)
        return res if res == true || res == false
        iter = res + 1
        return false if tokens[iter] == nil
      end
      n_token = tokens[iter]
      return true unless token.tk_val == n_token.tk_val
      iter
    elsif token.tk_type == :right
      true
    elsif token.tk_type == :vector_start
      iter += 1
      return false if tokens[iter] == nil
      until tokens[iter].tk_type == :right
        res = parsable?(tokens, iter)
        return res if res == true || res == false
        iter = res + 1
        return false if tokens[iter] == nil
      end
      n_token = tokens[iter]
      return true unless n_token.tk_val == 0
      iter
    elsif token.tk_type == :quote || token.tk_type == :unquote || token.tk_type == :quasiquote || token.tk_type == :"unquote-splicing"
      parsable?(tokens, iter + 1)
    else
      iter
    end
  end
end
