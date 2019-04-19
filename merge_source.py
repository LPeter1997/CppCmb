import glob
import os
import re

from enum import Enum

FILE_PREFIX = """
/**
 * cppcmb.hpp
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 *
 * Distributed under the MIT License.
 *
 * A simple to use C++17 parser combinator library.
 * Repository and usage: https://github.com/LPeter1997/CppCmb
 */
"""

INCLUDE_GUARD_PREFIX = 'CPPCMB'
SOURCE_PATH = 'source'
TARGET_PATH = 'cppcmb2.hpp'
TOP_INCLUDE = 'all.hpp'

STL_INCLUDE = r'^\s*#include\s*<.+>\s*\n'
LOCAL_INCLUDE = r'^\s*#include\s*\"(.+)\"'

stl_includes = set()
processed_files = set()

class ParseState(Enum):
    INITIAL = 0
    G_IFNDEF = 1
    G_DEFINE = 2
    G_ENDIF = 3

def process_file(root, fname):
    full_path = os.path.join(root, fname)
    if (full_path in processed_files):
        return ''

    processed_files.add(full_path)

    base = os.path.splitext(os.path.basename(full_path))[0].upper()
    g_ifndef = f'#ifndef {INCLUDE_GUARD_PREFIX}_{base}_HPP'
    g_define = f'#define {INCLUDE_GUARD_PREFIX}_{base}_HPP'
    g_endif = f'#endif /* {INCLUDE_GUARD_PREFIX}_{base}_HPP */'

    result = ''
    state = ParseState.INITIAL

    with open(full_path, 'r') as file:
        for line in file:
            if state == ParseState.INITIAL and g_ifndef in line:
                state = ParseState.G_IFNDEF
            elif state == ParseState.G_IFNDEF and g_define in line:
                state = ParseState.G_DEFINE
            elif state == ParseState.G_DEFINE:
                # Include guard already defined, we look at includes and such
                if re.match(STL_INCLUDE, line):
                    # A native include, we add it to the include set
                    stl_includes.add(' '.join(line.split()))
                elif re.match(LOCAL_INCLUDE, line):
                    # A library-include, we include it in the text
                    to_include = re.search(LOCAL_INCLUDE, line).group(1)
                    result += process_file(root, to_include)
                elif g_endif in line:
                    # Include guard ended, end of file
                    state = ParseState.G_ENDIF
                    break
                else:
                    # Just a simple line
                    result += line
        # State must be endif
        if state != ParseState.G_ENDIF:
            raise Exception(f'Invalid file "{full_path}"! State: {state}')
        return result

def main():
    content = process_file(SOURCE_PATH, TOP_INCLUDE)
    # Collapse newlines
    content = re.sub(r'\n\s*\n*', '\n\n', content).strip()
    # Create the prefix
    prefix = FILE_PREFIX.strip() + '\n\n'
    prefix += f'#ifndef {INCLUDE_GUARD_PREFIX}_HPP\n'
    prefix += f'#define {INCLUDE_GUARD_PREFIX}_HPP\n'
    # Create the postfix (actually called footer)
    postfix = f'#endif /* {INCLUDE_GUARD_PREFIX}_HPP */\n'
    # Create the includes
    incl = '\n'.join(sorted(stl_includes))
    # Write it to file
    with open(TARGET_PATH, 'w') as file:
        file.write(f'{prefix}\n{incl}\n\n{content}\n\n{postfix}')

if __name__ == "__main__":
    main()
