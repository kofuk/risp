project('risp', 'c', default_options : ['c_std=c11', 'warning_level=3'])

readline_dep = dependency('readline', required : true)

executable('risp', 'main.c', dependencies : readline_dep)
