# -*- coding: utf-8 -*-

module Evaluator
  SYNTAX_WORD = [:define,:set!,:if,:lambda,:let,:"let*",:letrec,:begin,:cond,:and,:or,:quote,:quasiquote,:unquote,:"unquote-splicing"]
  SUBROUTINE_WORD = [:list,:vector,:+,:-,:*,:"/",:"=",:<=,:>=,:<,:>,:not,:null?,:cons,:car,:cdr,:display,:exit,:load,:read,:eq?,:equal?,:eqv?,:write,:boolean,:"eof-object?",:boolean?,:number?,:string?,:symbol?,:pair?,:list?,:integer?,:complex?,:real?,:rational?,:max,:min,:exact,:inexact,:exact?,:inexact?,:"set-car!",:"set-cdr!",:__built_in_apply_function_flat__,:error]
  RESERVED_WORD = SYNTAX_WORD + SUBROUTINE_WORD

  def self.make_initial_environment(env)
    SYNTAX_WORD.each do |x|
      env.data[x] = Type::Value.new(:syntax, x)
    end
    SUBROUTINE_WORD.each do |x|
      env.data[x] = Type::Value.new(:subroutine, x)
    end
    env
  end

  def self.evaluate(exp, env, prop)
    # p exp
    # p env
    if exp.ex_type == :list       # exp is list
      return Type::Value.new(:list, []) if exp.ex_val.length == 0
      val = evaluate(exp.ex_val[0], env, prop)
      case val.vl_type
      when :syntax then
        return evaluate_syntax(val.vl_val, exp, env, prop)
      when :subroutine then
        return evaluate_subroutine(val.vl_val, exp.ex_val[1..-1].map{ |e| evaluate(e, env, prop) }, env, prop)
      when :closure then
        return evaluate_closure(val.vl_val, exp.ex_val[1..-1].map{ |e| evaluate(e, env, prop) }, env, prop)
      else
        raise Type::EvaluateError, "no procedure applied: #{val.vl_type}, #{val.vl_val}"
      end
    else                      # exp is atom
      case exp.ex_type
      when :integer,:float,:rational,:complex then
        return numeric_to_value(exp.ex_val)
      when :boolean,:character,:string then
        return Type::Value.new(exp.ex_type, exp.ex_val)
      when :token then
        associate(exp.ex_val, env)
      when :quote, :quasiquote, :vector then
        evaluate_quote(exp.ex_type, exp.ex_val, env, prop)
      else
        raise Type::EvaluateError, "invalid expression #{exp.ex_type}"
      end
    end
  end

  # ------------------------------ closure ------------------------------

  def self.evaluate_closure(closure, vals, env, prop)
    # p closure.name
    scope = {}
    if closure.args.instance_of?(Array)
      if closure.args.length > 0 && closure.args[-1].instance_of?(Array)
        raise Type::EvaluateError, "wrong number of arguments for #{closure.name != nil ? closure.name : "\#lambda"}" if vals.length < closure.args.length - 1
        for i in 0...(closure.args.length - 1)
          scope[closure.args[i]] = vals[i]
        end
        scope[closure.args[-1][0]] = Type::Value.new(:list, array_to_list(vals[(closure.args.length - 1)...(vals.length)]))
      else
        raise Type::EvaluateError, "wrong number of arguments for #{closure.name != nil ? closure.name : "\#lambda"}" unless vals.length == closure.args.length
        for i in 0...(closure.args.length)
          scope[closure.args[i]] = vals[i]
        end
      end
    else
      scope[closure.args] = Type::Value.new(:list, array_to_list(vals))
    end
    #  unless closure.name == nil
    #   scope[closure.name] = val
    #  end
    
    if closure.exps.length == 0
      return Type::Value.new(:undefined)
    else
      new_env = Type::Scope.new(scope, closure.env)
      for i in 0...(closure.exps.length - 1)
        evaluate(closure.exps[i], new_env, prop)
      end
      return evaluate(closure.exps[-1], new_env, prop)
    end
  end

  # ------------------------------ quote ------------------------------
  
  def self.evaluate_quote(key, exp, env, prop)
    ## puts "quote: key:{key}, exp:{exp}"
    if key == :vector
      vector = []
      for i in 0...(exp.length)
        vector.push(evaluate_quote(:quote, exp[i], env, prop))
      end
      return Type::Value.new(:vector, vector)
    end
    if exp.ex_type == :list
      es = exp.ex_val
      # return Type::Value.new(:list, []) if es.length == 0
      if es.length == 2 && es[0].ex_type == :token && (es[0].ex_val == :quote || es[0].ex_val == :quasiquote)
        return Type::Value.new(:quote, evaluate_quote(es[0].ex_val, es[1], env, prop))
      elsif es.length == 2 && es[0].ex_type == :token && es[0].ex_val == :unquote && key == :quasiquote
        return evaluate(es[1], env, prop)
      else
        array = []
        ispair = false
        for i in 0...(es.length)
          if i > 0 && i == es.length - 2 && es[i].ex_type == :period
            ispair = true
          elsif es[i].ex_type == :"unquote-splicing"
            splice = evaluate(es[i].ex_val, env, prop)
            raise Type::EvaluateError, "value of unquote-splicing isn't list" unless splice.vl_type == :list
            array += list_to_array(splice.vl_val)
          elsif es[i].ex_type == :list && es[i].ex_val.length == 2 && es[i].ex_val[0].ex_type == :token && es[i].ex_val[0].ex_val == :"unquote-splicing"
            splice = evaluate(es[i].ex_val[1], env, prop)
            raise Type::EvaluateError, "value of unquote-splicing isn't list" unless splice.vl_type == :list
            array += list_to_array(splice.vl_val)
          else
            array.push(evaluate_quote(key, es[i], env, prop))
          end
        end
        unless ispair
          return Type::Value.new(:list, array_to_list(array))
        else
          return array_to_pair(array)
        end
      end
    else
      case exp.ex_type
      when :integer, :float, :rational, :complex then
        return Type::Value.new(exp.ex_type, exp.ex_val)
      when :boolean, :character, :string then
        return Type::Value.new(exp.ex_type, exp.ex_val)
      when :token then
        return Type::Value.new(:symbol, exp.ex_val)
      when :quote, :quasiquote then
        return Type::Value.new(:quote, evaluate_quote(exp.ex_type, exp.ex_val, env, prop))
      when :vector then
        return evaluate_quote(exp.ex_type, exp.ex_val, env, prop)
      when :unquote then
        if key == :quasiquote
          return evaluate(exp.ex_val, env, prop)
        else
          raise Type::EvaluateError, "unquote appeared outside of quasi-quote"
        end
      else
        raise Type::EvaluateError, "invalid syntax in quote mode"
      end
    end
  end

  def self.variable_name_is_safe?(name)
    name.ex_type == :token && (! RESERVED_WORD.member?(name.ex_val))
  end

  # ------------------------------ syntax ------------------------------

  def self.evaluate_syntax(key, exp, env, prop)
    case key
    when :define then
      raise Type::EvaluateError, "invalid arguments for define" if exp.ex_val.length < 2
      token = exp.ex_val[1]
      if token.ex_type == :list
        tokens = token.ex_val                             # tokens : [ Expression ]
        raise Type::EvaluateError, "invalid definition for define" if tokens.length < 1
        fun_name = tokens[0]
        raise Type::EvaluateError, "invalid definition for define" unless variable_name_is_safe?(fun_name)
        args = []
        if tokens.length == 1
          
        elsif tokens.length == 3 && tokens[1].ex_type == :period
          raise Type::EvaluateError, "invalid variable for define" unless tokens[2].ex_type == :token
          args = tokens[2].ex_val
        else
          for i in 1...(tokens.length)
            if i == tokens.length - 2 && tokens[-2].ex_type == :period
              raise Type::EvaluateError, "invalid variable for define" unless tokens[-1].ex_type == :token
              raise Type::EvaluateError, "overlap variable for define" if args.member?(tokens[-1].ex_val)
              args.push([tokens[-1].ex_val])
              break
            else
              raise Type::EvaluateError, "invalid variable for define" unless tokens[i].ex_type == :token
              raise Type::EvaluateError, "overlap variable for define" if args.member?(tokens[i].ex_val)
              args.push(tokens[i].ex_val)
            end
          end
        end
        in_exps = []
        for i in 2...(exp.ex_val.length)
          in_exps.push(exp.ex_val[i])
        end
        env.data[fun_name.ex_val] = Type::Value.new(:closure, Type::Closure.new(fun_name.ex_val, args, in_exps, env))
        return Type::Value.new(:definition, fun_name.ex_val)
      else
        raise Type::EvaluateError, "invalid arguments for define" unless exp.ex_val.length == 3
        raise Type::EvaluateError, "invalid definition for define" unless variable_name_is_safe?(token)
        val = evaluate(exp.ex_val[2], env, prop)
        if val.vl_type == :closure
          val.vl_val.name = token.ex_val
        end
        env.data[token.ex_val] = val
        return Type::Value.new(:definition, token.ex_val)
      end
    when :set! then
      raise Type::EvaluateError, "wrong number of arguments for set!" unless exp.ex_val.length == 3
      raise Type::EvaluateError, "invalid variable for set!" unless variable_name_is_safe?(exp.ex_val[1])
      var = exp.ex_val[1].ex_val
      val = evaluate(exp.ex_val[2], env, prop)
      
      scope = env
      while scope
        if scope.data.member?(var)
          scope.data[var] = val
          return Type::Value.new(:undefined)
        end
        scope = scope.next
      end
      raise Type::EvaluateError, "no such variable #{var}"
    when :lambda then
      raise Type::EvaluateError, "invalid arguments for lambda" if exp.ex_val.length < 2
      tokens = exp.ex_val[1]
      in_exps = []
      args = []
      if tokens.ex_type == :list
        tokens = tokens.ex_val                        # tokens : [ Expression ]
        for i in 0...(tokens.length)
          if i == tokens.length - 2 && tokens[-2].ex_type == :period
            raise Type::EvaluateError, "invalid variable for lambda" unless tokens[-1].ex_type == :token
            raise Type::EvaluateError, "overlap variable for lambda" if args.member?(tokens[-1].ex_val)
            args.push([tokens[-1].ex_val])
            break
          else
            raise Type::EvaluateError, "invalid variable for lambda" unless variable_name_is_safe?(tokens[i])
            raise Type::EvaluateError, "overlap variable for lambda" if args.member?(tokens[i].ex_val)
            args.push(tokens[i].ex_val)
          end
        end
      elsif tokens.ex_type == :token
        args = tokens.ex_val
      else
        raise Type::EvaluateError, "invalid variable for lambda"
      end
      for i in 2...(exp.ex_val.length)
        in_exps.push(exp.ex_val[i])
      end
      return Type::Value.new(:closure, Type::Closure.new(nil, args, in_exps, env))
    when :if then
      len = exp.ex_val.length
      raise Type::EvaluateError, "invalid variable for if" unless len == 3 || len == 4
      flag = value_to_boolean(evaluate(exp.ex_val[1], env, prop))
      if flag
        return evaluate(exp.ex_val[2], env, prop)
      else
        if len == 3
          return Type::Value.new(:undefined)
        else
          return evaluate(exp.ex_val[3], env, prop)
        end
      end
    when :let then
      index = 1
      scope = {}
      loop_name = nil
      raise Type::EvaluateError, "few arguments for let" if exp.ex_val.length < 2
      if exp.ex_val[1].ex_type == :token           # let loop(( ))
        loop_name = exp.ex_val[1]
        raise Type::EvaluateError, "invalid variable for let" unless variable_name_is_safe?(loop_name)
        index += 1
      end
      raise Type::EvaluateError, "invalid syntax of let" unless exp.ex_val[index] && exp.ex_val[index].ex_type == :list
      vars = exp.ex_val[index].ex_val
      args = []
      for i in 0...(vars.length)
        raise Type::EvaluateError, "invalid variable definition" unless vars[i].ex_type == :list && vars[i].ex_val.length == 2 && vars[i].ex_val[0].ex_type == :token
        args.push(vars[i].ex_val[0].ex_val)
        scope[vars[i].ex_val[0].ex_val] = evaluate(vars[i].ex_val[1], env, prop)
      end
      index += 1
      exps = []
      for i in index...(exp.ex_val.length)
        exps.push(exp.ex_val[i])
      end
      new_env = Type::Scope.new(scope, env)
      if loop_name
        scope[loop_name.ex_val] = Type::Value.new(:closure, Type::Closure.new(loop_name.ex_val, args, exps, new_env))
      end
      return Type::Value.new(:undefined) unless exp.ex_val[index]
      for i in index...(exp.ex_val.length - 1)
        evaluate(exp.ex_val[i], new_env, prop)
      end
      return evaluate(exp.ex_val[-1], new_env, prop)
    when :"let*" then
      raise Type::EvaluateError, "few arguments for let*" if exp.ex_val.length < 2
      raise Type::EvaluateError, "invalid syntax for let*" unless exp.ex_val[1].ex_type == :list
      rests = exp.ex_val[1].ex_val
      new_env = nil
      if rests.length == 0
        new_env = Type::Scope.new({}, env)
      else
        new_env = env
      end
      for i in 0...(rests.length)
        raise Type::EvaluateError, "invalid syntax for let*" unless rests[i].ex_type == :list && rests[i].ex_val.length == 2 && rests[i].ex_val[0].ex_type == :token
        scope = {}
        scope[rests[i].ex_val[0].ex_val] = evaluate(rests[i].ex_val[1], new_env, prop)
        new_env = Type::Scope.new(scope, new_env)
      end
      return Type::Value.new(:undefined) if exp.ex_val.length == 2
      for i in 2...(exp.ex_val.length - 1)
        evaluate(exp.ex_val[i], new_env, prop)
      end
      return evaluate(exp.ex_val[-1], new_env, prop)
    when :begin then
      return Type::Value.new(:undefined) if exp.ex_val.length < 2
      for i in 1...(exp.ex_val.length - 1)
        evaluate(exp.ex_val[i], env, prop)
      end
      return evaluate(exp.ex_val[-1], env, prop)
    when :cond then
      raise Type::EvaluateError, "few arguments for cond" if exp.ex_val.length < 2
      es = exp.ex_val
      for i in 1...(es.length)
        raise Type::EvaluateError, "invalid syntax for cond" unless es[i].ex_type == :list
        len = es[i].ex_val.length
        raise Type::EvaluateError, "invalid syntax for cond" if len == 0
        if es[i].ex_val[0].ex_type == :token && es[i].ex_val[0].ex_val == :else
          raise Type::EvaluateError, "unexpected else for cond" unless es.length - 1 == i
          if len == 1
            return Type::Value.new(:undefined)
          else
            for j in 1...(len - 1)
              evaluate(es[i].ex_val[j], env, prop)
            end
            return evaluate(es[i].ex_val[-1], env, prop)
          end
        end
        if value_to_boolean(evaluate(es[i].ex_val[0], env, prop))
          if len == 1
            return Type::Value.new(:undefined)
          else
            for j in 1...(len - 1)
              evaluate(es[i].ex_val[j], env, prop)
            end
            return evaluate(es[i].ex_val[-1], env, prop)
          end
        end
      end
      return Type::Value.new(:undefined)
    when :and, :or then
      return Type::Value.new(:boolean, { :and => true, :or => false }[key]) if exp.ex_val.length < 2
      for i in 1...(exp.ex_val.length - 1)
        val = evaluate(exp.ex_val[i], env, prop)
        return val if key == :and && ! value_to_boolean(val)
        return val if key == :or && value_to_boolean(val)
      end
      return evaluate(exp.ex_val[-1], env, prop)
    when :quote, :quasiquote then
      raise Type::EvaluateError, "wrong number of arguments for quote" unless exp.ex_val.length == 2
      return evaluate_quote(key, exp.ex_val[1], env, prop)
    else
      raise Type::EvaluateError, "no such syntax"
    end
  end

  # ------------------------------ subroutine ------------------------------
  
  def self.evaluate_subroutine(key, vals, env, prop)
    case key
    when :list then
      return Type::Value.new(:list, array_to_list(vals))
    when :vector then
      return Type::Value.new(:vector, vals)
    when :+, :* then
      res = { :+ => 0, :* => 1 }[key]
      for i in 0...(vals.length)
        res = res.__send__(key, value_to_numeric(vals[i]))
      end
      return numeric_to_value(res)
    when :-, :"/" then
      raise Type::EvaluateError, "few arguments for #{key}" if vals.length < 1
      res = value_to_numeric(vals[0])
      res = res.to_r if res.is_a?(Integer)
      return numeric_to_value({ :- => - res, :"/" => 1 / res }[key]) if vals.length == 1
      for i in 1...(vals.length)
        res = res.__send__(key, value_to_numeric(vals[i]))
      end
      return numeric_to_value(res)
    when :"=" then
      raise Type::EvaluateError, "few arguments for #{key}" if vals.length < 2
      num = value_to_numeric(vals[0])
      for i in 1...(vals.length)
        new_num = value_to_numeric(vals[i])
        unless num.__send__(:"==", new_num)
          return Type::Value.new(:boolean, false)
        end          
      end
      return Type::Value.new(:boolean, true)
    when :<, :>, :>=, :<= then
      raise Type::EvaluateError, "few arguments for #{key}" if vals.length < 2
      num = value_to_numeric(vals[0])
      return Type::EvaluateError, "real number required, but got #{num}" if num.vl_type == :complex
      for i in 1...(vals.length)
        new_num = value_to_numeric(vals[i])
        return Type::EvaluateError, "real number required, but got #{new_num}" if new_num.vl_type == :complex
        unless num.__send__(key, new_num)
          return Type::Value.new(:boolean, false)
        else
          num = new_num
        end
      end
      return Type::Value.new(:boolean, true)
    when :max, :min then
      raise Type::EvaluateError, "few arguments for #{key}" if vals.length < 1
      num = value_to_numeric(vals[0])
      return Type::EvaluateError, "real number required, but got #{num}" if num.vl_type == :complex
      for i in 1...(vals.length)
        new_num = value_to_numeric(vals[i])
        return Type::EvaluateError, "real number required, but got #{new_num}" if new_num.vl_type == :complex
        if num.__send__({ :max => :<, :min => :> }[key], new_num)
          num = new_num
        end
      end
      return numeric_to_value(num)
    when :not then
      raise Type::EvaluateError, "wrong number of arguments for not" unless vals.length == 1
      return Type::Value.new(:boolean, ! value_to_boolean(vals[0]))
    when :null? then
      raise Type::EvaluateError, "wrong number of arguments for null?" unless vals.length == 1
      return Type::Value.new(:boolean, vals[0].vl_type == :list && vals[0].vl_val.empty?)
    when :cons then
      raise Type::EvaluateError, "wrong number of arguments for cons" unless vals.length == 2
      if vals[1].vl_type == :list
        return Type::Value.new(:list, [vals[0], vals[1].vl_val])
      else
        return Type::Value.new(:pair, [vals[0], vals[1]])
      end
    when :car then
      raise Type::EvaluateError, "wrong number of arguments for car" unless vals.length == 1
      if vals[0].vl_type == :list
        raise Type::EvaluateError, "empty list is applied to car" if vals[0].vl_val.length == 0
        return vals[0].vl_val[0]
      elsif vals[0].vl_type == :pair
        return vals[0].vl_val[0]
      elsif vals[0].vl_type == :quote
        return Type::Value.new(:symbol, :quote)
      else
        raise Type::EvaluateError, "argument isn't pair: car"
      end
    when :cdr then
      raise Type::EvaluateError, "wrong number of arguments for cdr" unless vals.length == 1
      if vals[0].vl_type == :list
        raise Type::EvaluateError, "empty list is applied to cdr" if vals[0].vl_val.length == 0
        return Type::Value.new(:list, vals[0].vl_val[1])
      elsif vals[0].vl_type == :pair
        return vals[0].vl_val[1]
      elsif vals[0].vl_type == :quote
        return Type::Value.new(:list, [vals[0].vl_val, []])
      else
        raise Type::EvaluateError, "argument isn't pair: cdr"
      end
    when :display then
      raise Type::EvaluateError, "wrong number of arguments for display" unless vals.length == 1
      case vals[0].vl_type
      when :string, :character then
        print vals[0].vl_val
      else
        print vals[0].to_s
      end
      return Type::Value.new(:undefined)
    when :write then
      raise Type::EvaluateError, "wrong number of arguments for write" unless vals.length == 1
      print vals[0].to_s
      return Type::Value.new(:undefined)
    when :read then
      raise Type::EvaluateError, "wrong number of arguments for read" if vals.length > 1
      buf = prop[:buffer]
      until Parser::parsable?(buf[:tokens])
        line = gets
        return Type::Value.new(:end_of_file) unless line
        buf[:input] += line.each_char.to_a
        until buf[:input].length == 0
          begin
            Parser::lex(buf)
          rescue Type::LexError => e
            raise Type::EvaluateError, "lex in read: #{e.message}"
          end
        end
      end
      begin
        read_exp = Parser::parse(buf[:tokens])
      rescue Type::ParseError => e
        raise Type::EvaluateError, "parse in read: #{e.message}"
      end
      return evaluate_quote(:quote, read_exp, env, prop)
    when :eq?, :equal? then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 2
      return Type::Value.new(:boolean, false) unless vals[0].vl_type == vals[1].vl_type
      if key == :eq?
        return Type::Value.new(:boolean, vals[0].vl_val.__id__ == vals[1].vl_val.__id__)
      else
        return Type::Value.new(:boolean, vals[0].vl_val == vals[1].vl_val)
      end
    when :eqv? then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 2
      return Type::Value.new(:boolean, false) unless vals[0].vl_type == vals[1].vl_type
      case vals[0].vl_type
      when :integer,:float,:rational,:complex then
        return Type::Value.new(:boolean, vals[0].vl_val == vals[1].vl_val)
      when :boolean then
        return Type::Value.new(:boolean, vals[0].vl_val == vals[1].vl_val)
      when :symbol then
        return Type::Value.new(:boolean, vals[0].vl_val == vals[1].vl_val)
      when :character then
        return Type::Value.new(:boolean, vals[0].vl_val == vals[1].vl_val)
      when :list then
        if vals[0].vl_val.length == 0 && vals[1].vl_val.length == 0
          return Type::Value.new(:boolean, true)
        else
          return Type::Value.new(:boolean, vals[0].vl_val.__id__ == vals[1].vl_val.__id__)
        end
      when :pair, :vector, :string, :closure then
        return Type::Value.new(:boolean, vals[0].vl_val.__id__ == vals[1].vl_val.__id__)
      else
        return Type::Value.new(:boolean, false)
      end
    when :load then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 1
      path = vals[0]
      raise Type::EvaluateError, "invalid argument for load" unless path.vl_type == :string
      raise Type::EvaluateError, "file isn't exist" unless File.file?(path.vl_val)
      buf = { :mode => :plain, :tokens => [], :input => [], :store => "" }
      file = nil
      begin
        file = File.open(path.vl_val)
      rescue Exception
        raise Type::EvaluateError, "file open error"
      end
      begin
        while true
          line = file.gets
          return Type::Value.new(:boolean, true) unless line
          buf[:input] += line.each_char.to_a
          Parser::lex(buf)
          if Parser::parsable?(buf[:tokens])
            exp = Parser::parse(buf[:tokens])
            val = Evaluator::evaluate(exp, env, prop)
          end
        end
      rescue Type::LexError, Type::ParseError, Type::EvaluateError => e
        raise Type::EvaluateError, "error in load"
      end
    when :"eof-object?",:boolean?,:list?,:pair?,:number?,:string?,:symbol?,:integer?,:complex?,:real?,:rational? then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 1
      case key
      when :pair? then
        return Type::Value.new(:boolean, vals[0].vl_type == :pair || (vals[0].vl_type == :list && vals[0].vl_val.length > 0))
      when :number? then
        return Type::Value.new(:boolean, vals[0].vl_type == :integer || vals[0].vl_type == :float || vals[0].vl_type == :rational || vals[0].vl_type == :complex)
      when :list? then
        return Type::Value.new(:boolean, vals[0].vl_type == :list || vals[0].vl_type == :quote)
      when :real? then
        return Type::Value.new(:boolean, vals[0].vl_type == :integer || vals[0].vl_type == :float || vals[0].vl_type == :rational)
      else
        return Type::Value.new(:boolean, vals[0].vl_type == { :"eof-object?" => :end_of_file , :boolean? => :boolean, :list? => :list, :string? => :string, :symbol? => :symbol, :integer? => :integer, :complex? => :complex, :rational? => :rational }[key])
      end
    when :exact?, :inexact? then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 1
      case vals[0].vl_type
      when :integer, :rational then
        return Type::Value.new(:boolean, key == :exact?)
      when :float then
        return Type::Value.new(:boolean, key == :inexact?)
      else
        return Type::Value.new(:boolean, false)
      end
    when :exact then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 1
      case vals[0].vl_type
      when :float then
        return numeric_to_value(vals[0].vl_val.to_r)
      when :integer, :rational then
        return vals[0]
      else
        raise "real number required, but got #{vals[0]}"
      end
    when :inexact then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 1
      case vals[0].vl_type
      when :integer, :rational then
        return numeric_to_value(vals[0].vl_val.to_f)
      when :float then
        return vals[0]
      else
        raise "real number required, but got #{vals[0]}"
      end
    when :"set-car!", :"set-cdr!" then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 2
      raise Type::EvaluateError, "invalid application to #{key}" unless (vals[0].vl_type == :list && vals[0].vl_val.length > 0) || vals[0].vl_type == :pair || vals[0].vl_type == :quote
      if key == :"set-car!"
        if vals[0].vl_type == :list
          vals[0].set!(:list, [vals[1], vals[0].vl_val[1]])
        elsif vals[0].vl_type == :pair
          vals[0].set!(:pair, [vals[1], vals[0].vl_val[1]])
        else
          vals[0].set!(:list, [vals[1], [vals[0].vl_val, []]])
        end
      else
        if vals[1].vl_type == :list
          if vals[0].vl_type == :list || vals[0].vl_type == :pair
            vals[0].set!(:list, [vals[0].vl_val[0], vals[1].vl_val])
          else
            vals[0].set!(:list, [Type::Value.new(:symbol, :quote), vals[1]])
          end
        else
          if vals[0].vl_type == :list || vals[0].vl_type == :pair
            vals[0].set!(:pair, [vals[0].vl_val[0], vals[1]])
          else
            vals[0].set!(:pair, [Type::Value.new(:symbol, :quote), vals[1]])
          end
        end
      end
      return Type::Value.new(:undefined)
    when :__built_in_apply_function_flat__ then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length == 2
      raise Type::EvaluateError, "argument isn't list: #{key}" unless vals[1].vl_type == :list
      if vals[0].vl_type == :subroutine
        return evaluate_subroutine(vals[0].vl_val, list_to_array(vals[1].vl_val), env, prop)
      elsif vals[0].vl_type == :closure
        return evaluate_closure(vals[0].vl_val, list_to_array(vals[1].vl_val), env, prop)
      else
        raise Type::EvaluateError, "#{key}"
      end
    when :error then
      raise Type::EvaluateError, "wrong number of arguments for #{key}" unless vals.length < 1
      raise Type::EvaluateError, "error: #{key}"
    when :exit then
      raise Type::ExitException
    end
    
  end

  # ------------------------------ convert ------------------------------
  
  def self.numeric_to_value(x)
    if x.is_a?(Integer)
      return Type::Value.new(:integer, x)
    elsif x.is_a?(Float)
      return Type::Value.new(:float, x)
    elsif x.is_a?(Rational)
      if x.zero? || x.denominator == 1
        return Type::Value.new(:integer, x.to_i)
      else
        return Type::Value.new(:rational, x)
      end
    elsif x.is_a?(Complex)
      if x.imaginary == 0
        return Type::Value.new(:float, x.real.to_f)
      else
        return Type::Value.new(:complex, Complex(x.real.to_f, x.imaginary.to_f))
      end
    else
      raise Type::EvaluateError, "numeric_to_value"
    end
  end
  
  def self.value_to_numeric(v)
    case v.vl_type
    when :integer, :float, :rational, :complex then
      v.vl_val
    else
      raise Type::EvaluateError, "value_to_numeric"
    end
  end

  def self.value_to_boolean(val)
    ! (val.vl_type == :boolean && val.vl_val == false)
  end

  def self.array_to_list(a)
    l = []
    b = a.reverse
    for i in 0...(b.length)
      l = [b[i], l]
    end
    l
  end

  def self.list_to_array(l)
    a = []
    until l == []
      a.push(l[0])
      l = l[1]
    end
    a
  end

  def self.array_to_pair(a)
    b = a.reverse
    p = b[0]
    for i in 1...(b.length)
      p = Type::Value.new(:pair, [b[i], p])
    end
    p
  end

  # ------------------------------ environment ------------------------------

  def self.key_exist?(key, env)
    scope = env
    while scope
      if scope.data.has_key?(key)
        return true
      else
        scope = scope.next
      end
    end
    false
  end

  def self.associate(key, env)
    scope = env
    while scope
      if scope.data.has_key?(key)
        return scope.data[key]
      else
        scope = scope.next
      end
    end
    raise Type::EvaluateError, "no such variable #{key}"
  end
   
end
