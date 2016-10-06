# -*- coding: utf-8 -*-

$LOAD_PATH.push('.')

require 'type.rb'
require 'parser.rb'
require 'evaluator.rb'

FILE_NAME = "init.scm"

module SchemeInterpreter
  def self.initialize_scheme(env)
    buf = { :mode => :plain, :tokens => [], :store => "", :input => [] }
    prop = { :buffer => buf }
    file = nil
    begin
      file = File::open(FILE_NAME)
    rescue Exception
      return
    end
    begin
      while true
        until Parser::parsable?(buf[:tokens])
          line = file.gets
          unless line
            file.close
            return
          end
          buf[:input] += line.each_char.to_a
          until buf[:input].length == 0
            begin
              Parser::lex(buf)
            rescue Type::LexError
              buf[:mode] = :plain
              buf[:store] = ""
            end
          end
        end
        while Parser::parsable?(buf[:tokens])
          begin
            exp = Parser::parse(buf[:tokens])
            val = Evaluator::evaluate(exp, env, prop)
          rescue Type::ParseError, Type::EvaluateError
          end
        end
      end
    rescue Exception
      file.close
      return 
    end
  end
  
  def self.main
    buf = { :mode => :plain, :tokens => [], :store => "", :input => [] }
    prop = { :buffer => buf }
    env = Evaluator::make_initial_environment(Type::Scope.new)
    begin
      initialize_scheme(env)
    rescue Exception
      puts "initialize error"
    end
    while true
      until Parser::parsable?(buf[:tokens])
        print (buf[:tokens].length == 0 && buf[:mode] == :plain ? "scheme$ " : "..> ")
        line = gets
        if line == nil
          puts ""
          return
        end
        buf[:input] += line.each_char.to_a
        until buf[:input].length == 0
          begin
            Parser::lex(buf)
          rescue Type::LexError => e
            puts ("-*" * 15 + "-")
            puts "LexError: #{e.message}"
            puts ("-*" * 15 + "-")
            buf[:mode] = :plain
            buf[:store] = ""
          end
        end
      end
      
      while Parser::parsable?(buf[:tokens]) 
        begin
          exp = Parser::parse(buf[:tokens])
          val = Evaluator::evaluate(exp, env, prop)
          puts "==> #{val}"
        rescue Type::ParseError => e
          puts ("-=" * 15 + "-")
          puts "ParseError: #{e.message}"
          puts ("-=" * 15 + "-")
        rescue Type::EvaluateError => e
          puts ("-+" * 15 + "-")
          puts "EvaluateError: #{e.message}"
          puts ("-+" * 15 + "-")
        rescue SystemStackError => e
          puts ("-@" * 15 + "-")
          puts "SystemStackError: Stack Overflow"
          puts ("-@" * 15 + "-")
        rescue Type::ExitException
          return
        end
      end
    end
  end

  class Scheme
    def initialize
      @env = Evaluator::make_initial_environment(Type::Scope.new)
      begin
        initialize_scheme(@env)
      rescue Exception
        puts "scheme initialize"
      end
    end
    
    def initialize_scheme(env)
      buf = { :mode => :plain, :tokens => [], :store => "", :input => [] }
      prop = { :buffer => buf }
      file = nil
      begin
        file = File::open(FILE_NAME)
      rescue Exception
        return
      end
      begin
        while true
          until Parser::parsable?(buf[:tokens])
            line = file.gets
            unless line
              file.close
              return
            end
            buf[:input] += line.each_char.to_a
            until buf[:input].length == 0
              begin
                Parser::lex(buf)
              rescue Type::LexError
                buf[:mode] = :plain
                buf[:store] = ""
              end
            end
          end
          while Parser::parsable?(buf[:tokens])
            begin
              exp = Parser::parse(buf[:tokens])
              val = Evaluator::evaluate(exp, env, prop)
            rescue Type::ParseError, Type::EvaluateError
            end
          end
        end
      rescue Exception
        file.close
        return 
      end
    end

    def execute(s)
      buf = { :mode => :plain, :tokens => [], :store => "", :input => [] }
      prop = { :buffer => buf }
      buf[:input] = s.each_char.to_a + ["\n"]
      begin
        Parser::lex(buf)
        exp = Parser::parse(buf[:tokens])
        val = Evaluator::evaluate(exp, @env, prop)
        val.to_s
      rescue Type::LexError => e
        e.message
      rescue Type::ParseError => e
        e.message
      rescue Type::EvaluateError => e
        e.message
      rescue SystemStackError => e
        "SystemStackError: Stack Overflow"
      rescue Type::ExitException
        @env = Evaluator::make_initial_environment(Type::Scope.new)
        begin
          initialize_scheme(@env)
          "[environment initialized]"
        rescue Exception
          "Error: initialize"
        end
      end
    end
  end
end

if $0 == __FILE__
  SchemeInterpreter::main
end
