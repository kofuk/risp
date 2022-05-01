project('risp', 'c', default_options : ['c_std=c11', 'warning_level=3'])

readline_dep = dependency('readline', required : true)

risp_src = [
  'main.c',
  'parse.c',
  'primitive.c',
  'rt.c'
]
executable('risp', sources : risp_src, dependencies : readline_dep)
