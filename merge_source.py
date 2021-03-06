import datetime
import glob
import os
import re

from enum import Enum

INCLUDE_GUARD_PREFIX = 'CPPCMB'
SOURCE_PATH = 'source'
TARGET_PATH = 'cppcmb.hpp'
TOP_INCLUDE = 'cppcmb.hpp'

FILE_PREFIX = f"""
/**
 * {TARGET_PATH}
 *
 * This file has been merged from multiple source files.
 * Generation date: {datetime.datetime.now()}
 *
 * Copyright (c) 2018-2019 Peter Lenkefi
 * Distributed under the MIT License.
 *
 * A simple to use C++17 parser combinator library.
 * Repository and usage: https://github.com/LPeter1997/CppCmb
 */
"""

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
    full_root = os.path.realpath(SOURCE_PATH)
    full_path = os.path.join(root, fname)
    canon_path = os.path.realpath(full_path)
    if (canon_path in processed_files):
        return ''

    processed_files.add(canon_path)

    #base = os.path.splitext(os.path.basename(full_path))[0].upper()

    # Determinde what comes in between the prefix and HPP
    center_part = os.path.relpath(canon_path, full_root)
    if center_part[-4:] != '.hpp':
        raise Exception(f'Invalid extension for file "{canon_path}"')
    center_part = center_part[:-4].replace('\\', '_').replace('/', '_').upper()

    g_ifndef = fr'#ifndef {INCLUDE_GUARD_PREFIX}_{center_part}_HPP'
    g_define = fr'#define {INCLUDE_GUARD_PREFIX}_{center_part}_HPP'
    g_endif = fr'#endif \/\* {INCLUDE_GUARD_PREFIX}_{center_part}_HPP \*\/'

    result = ''
    state = ParseState.INITIAL

    with open(full_path, 'r') as file:
        for line in file:
            if state == ParseState.INITIAL and re.match(g_ifndef, line):
                state = ParseState.G_IFNDEF
            elif state == ParseState.G_IFNDEF and re.match(g_define, line):
                state = ParseState.G_DEFINE
            elif state == ParseState.G_DEFINE:
                # Include guard already defined, we look at includes and such
                if re.match(STL_INCLUDE, line):
                    # A native include, we add it to the include set
                    stl_includes.add(' '.join(line.split()))
                elif re.match(LOCAL_INCLUDE, line):
                    # A library-include, we include it in the text
                    to_include = re.search(LOCAL_INCLUDE, line).group(1)
                    to_include_path = os.path.dirname(to_include)
                    to_include_fname = os.path.basename(to_include)
                    next_root = os.path.join(root, to_include_path)
                    result += process_file(next_root, to_include_fname)
                elif re.match(g_endif, line):
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
    try:
        content = process_file(SOURCE_PATH, TOP_INCLUDE)
        # Collapse newlines
        content = re.sub(r'\n\n+', '\n\n', content).strip()
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
        print(f'Files merged successfully into "{TARGET_PATH}"!')
    except Exception as err:
        print(f'An error occured during merge:\n{err}')

if __name__ == "__main__":
    main()
