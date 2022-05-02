project('risp', 'c', default_options : ['c_std=c11', 'warning_level=3'])

definitions = []

if get_option('readline').enabled()
  readline_dep = dependency('readline', required : true)
  definitions += '-DHAVE_READLINE'
else
  readline_dep = declare_dependency()
endif

risp_src = [
  'main.c',
  'parse.c',
  'primitive.c',
  'rt.c'
]
executable(
  'risp',
  sources : risp_src,
  dependencies : readline_dep,
  c_args: definitions)
