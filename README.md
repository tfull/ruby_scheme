ruby_scheme
===========

内容  
type.rb ==&gt; クラスの定義  
parser.rb ==&gt; 字句解析および構文解析  
evaluator.rb ==&gt; 評価  
main.rb ==&gt; 繰り返し処理  
init.scm ==&gt; 起動時に読み込むライブラリ

実行  
shell$ ruby main.rb

動作例  
scheme$ 1  
==&gt; 1  
scheme$ ((lambda (x) (+ x 1)) 10)  
==&gt; 11  
...

