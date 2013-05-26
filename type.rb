# -*- coding: utf-8 -*-

module Type
  class LexError < Exception ; end
  class ParseError < Exception ; end
  class EvaluateError < Exception ; end
  class ExitException < Exception ; end

  class Scope
    attr_accessor :data, :next
    
    def initialize(data = {}, ne = nil)
      @data = data
      @next = ne
    end

    def to_s
      if @next
        "#{@data} -> #{@next}"
      else
        "#{@data}"
      end
    end
  end
  
  class Token
    attr_reader :tk_type, :tk_val
    
    def initialize(type, val = nil)
      @tk_type = type
      @tk_val = val
    end

    def to_s
      if @tk_val == nil
        "Token(#{@tk_type})"
      else
        "Token(#{@tk_type}, #{@tk_val})"
      end
    end
  end

  class Expression
    attr_reader :ex_type, :ex_val
    
    def initialize(type, val = nil)
      @ex_type = type
      @ex_val = val
    end
    
    def to_s
      if @ex_val == nil
        "Expression(#{@ex_type})"
      else
        "Expression(#{@ex_type}, #{@ex_val})"
      end
    end
  end

  class Value
    attr_reader :vl_type, :vl_val

    def initialize(type, val = nil)
      @vl_type = type
      @vl_val = val
    end

    def set!(type, val)
      @vl_type = type
      @vl_val = val
    end

    def ==(o)
      return false unless @vl_type == o.vl_type
      @vl_val == o.vl_val
    end
    
    def to_s
      case @vl_type
      when :integer,:float,:rational,:complex,:closure then
        @vl_val.to_s
      when :boolean then
        @vl_val ? "\#t" : "\#f"
      when :character then
        case @vl_val
        when :"\n" then
          "\#\\newline"
        when :"\t" then
          "\#\\tab"
        when :" " then
          "\#\\space"
        else
          "\#\\#{@vl_val}"
        end
      when :string then
        "\"" + @vl_val.gsub(/\n/, "\\n").gsub(/\t/, "\\t").gsub(/\\/, "\\\\").gsub(/\"/, "\\\"") + "\""
      when :symbol then
        @vl_val.to_s
      when :list then
        if @vl_val == []
          "()"
        else
          s = "("
          l = @vl_val
          while true
            s += l[0].to_s
            if l[1] != []
              s += " "
              l = l[1]
            else
              break
            end
          end
          s + ")"
        end
      when :pair then
        s = "("
        s += @vl_val[0].to_s + " "
        v = @vl_val[1]
        while v.vl_type == :pair
          s += v.vl_val[0].to_s + " "
          v = v.vl_val[1]
        end
        s + ". " + v.to_s + ")"
      when :vector then
        if @vl_val.length == 0
          "\#()"
        else
          s = "\#("
          for i in 0...(@vl_val.length - 1)
            s += @vl_val[i].to_s + " "
          end
          s + @vl_val[-1].to_s + ")"
        end
      when :quote then
        "'#{@vl_val}"
      when :syntax,:subroutine then
        "\#<#{@vl_type} #{@vl_val}>"
      when :definition then
        "|define #{@vl_val}|"
      when :undefined then
        "\#<undefined>"
      when :end_of_file then
        "\#<end of file>"
      else
        "undefined value to_s"
      end
    end
  end

  class Closure
    attr_reader :args, :exps, :env
    attr_accessor :name
    
    def initialize(name, args, exps, env)
      @name = name
      @args = args
      @exps = exps
      @env = env
    end

    def to_s
      unless @name
        "\#<closure \#lambda>"
      else
        "\#<closure #{@name}>"
      end
    end
  end
end
