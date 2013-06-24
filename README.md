Tapas
=====

Dalvik bytecode analysis in scala

Semantic Grep
=============

The current version of Tapas is focused on providing a "semantic grep" for
determining the set of application methods that call a set of flagged methods
and associating a cost with each of these methods based on category.


Running Semantic Grep
---------------------

The easiest way to run the semantic grep is by using [sbt: the simple build
tool](http://www.scala-sbt.org "SBT") for Scala.  From the Tapas directory:

```
$ sbt
[info] ...
> 
'''

At the prompt, you can compile and run the project with ```run''':

```
> run
[info] Running com.ucombinator.dalvik.Analyzer 
usage: analyzer [<options>] APK-file
  -h | --help              :: print this message
  -d | --dump              :: dump out the class definitions
  -o | --output-file       :: set the file for dump
  -c | --class-name        :: indicate the class name to analyze
  -m | --method-name       :: indicate the method name to analyze
  -f | --config            :: set the configuration file
  -l | --list-categories   :: list the known categories of sources and sinks
  -g | --limit-categories  :: limit report to only contain certain categories
  -G | --categories-file   :: limit report to categories in file
  -a | --add-methods       :: allows adding a set of methods w/categories
  -A | --add-method-file   :: specify a filename of additional methods w/categories
  -s | --specify-cost      :: specifies cost by category
  -S | --specify-cost-file :: specify a filename of costs by category
[success] Total time: 7 s, completed Jun 24, 2013 2:25:21 PM
>
'''

To have the analyzer analyze a file, simple provide the APK file along with the run line:

```
> run <path-to-apks>/MyDrawA.apk
[info] Running com.ucombinator.dalvik.Analyzer <path-to-apks>/MyDrawA.apk
Sorted methods by cost: 
  5	com.invincea.draw.MyDrawService.onCreate [MyDrawService.java at line: 21 pos: 0]
  5	com.invincea.draw.MyDrawService.onDestroy [MyDrawService.java at line: 32 pos: 0]

[success] Total time: 2 s, completed Jun 24, 2013 2:30:23 PM
'''

Configuring Sematic Grep
------------------------

The list of methods that the analyzer looks for calls to is configured in the
XML file: ```config/sourceSink.xml'''.  Alternatively, a configuration file can
be specified using the ```--config''' option.  Additional methods can also be
specified, either on the command line or through an additional file listing
methods and categories (see Semantic Grep Options below).

Semantic Grep Options
---------------------

- ```-h''' or ```--help''' : prints the help message listing all the options.
- ```-d''' or ```--dump''' : dumps the Dalvik bytecode in assembly language
  form for each method defined in the supplied APK file.  The output is dumped
  to standard out unless an output file is specified.
- ```-o''' or ```--output-file''' : specifies the output file to push output of
  analysis and dump (if ```--dump''' is used).  If left unspecified, output is
  sent to standard out.
- ```-c''' or ```--class-name''' : *Deprecated* specifies a class name to
  analyze.  This causes the analyzer to perform a different analysis instead of
  semantic grep.
- ```-m''' or ```--method-name''' : *Deprecated* indicate the method name to
  analyze.  This causes the analyzer to perform a different analysis from
  sematic grep.
- ```-f''' or ```--config''' : specifies a configuration file to use in place
  of the default configuration file in
  ```config/sourceSink.xml'''sourceSink.xml'''.
- ```-l''' or ```--list-categories''' : when no APK file is specified, this
  option lists the known categories of flagged methods in the current
  configuration file and for any additional methods provided by the user.
- ```-g''' or ```--limit-categories''' : limits the report to only contain
  flagged methods from the given categories.  Categories are space separated,
  and if more then one category is specified, then categories should be
  surrounded with curly braces.  For example:
     ```
     > run --limit-categories { ipc filesystem } <APK file>
     '''
- ```-G''' or ```--categories-file''' : similar to ```-g''' or
  ```-limit-categories''', except that categories are specified in a text file
  (one category per line) with ```#''' used to indicate comments
- ```-a''' or ```--add-methods''' : allows adding a set of methods with
  categories at the command line to flag in addition to those already specified
  in a configuration file.  This allows extra methods to be listed for analyzing
  a particular application.  The class and method name are separated from the
  category with a comma, and if more then one is listed, it must be surrounded by
  curly braces with spaces between each method and category pair.  For example:
     ```
     > run --add-methods { java.io.File.delete, filesystem java.io.File.open, filesystem } <APK file>
     '''
- ```-A''' or ```--add-method-file''' : similar to ```-a''' or
  ```-add-methods''' except methods and categories are listed in a file.  Each
  line contains a single method and category, either separated by space or comma.
- ```-s''' or ```--specify-cost''' : specifies the cost to associate with each
  category of flagged method.  The category is listed first, then the cost,
  separated by a comma.  If more than one is specified, they are space separated
  and should be surrounded by curly braces.  For example,
     ```
     > run --specify-cost { filesystem,10 log,1 } <APK file>
     '''
  Any category that does not have an associated cost is assigned the default
  cost of 5.
- ```-S''' or ```--specify-cost-file''' : similar to ```-s''' or
  ```--specify-cost''', except the costs are specified in a file.  Each line in
  the file has a category and a cost, either separated by comma or space.

