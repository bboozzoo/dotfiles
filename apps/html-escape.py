#!/usr/bin/env python
import getopt
import sys
import string

def print_usage():
    print '''escape characters for HTML - html-escape.py
Usage: html-escape.py [-i in-file] [-h]
    -i in-file   read input from file
    -h           show help
'''
    
HTML_escape_entries = {
    '&' : '&amp',
    '<' : '&lt',
    '>' : '&gt',
    '"' : '&quot',
    '\'' : '&apos'
}

def html_escape(in_str):
    """
    do the actual escaping
    """
    return string.join((HTML_escape_entries.get(x, x) for x in in_str), '')
    

if __name__ == '__main__':
    input_file_name = None # will override reading from stdin
    in_file = sys.stdin # default file == sys.stdin

    try:
        optlist, args = getopt.getopt(sys.argv[1:], 'hi:')
    except getopt.GetoptError, e:
        print_usage()
        sys.exit(1)
    
    for opt, optarg in optlist:
        if opt == '-i':
            input_file_name = optarg
        elif opt == '-h':
            print_usage()
            sys.exit(0)
    
    if input_file_name != None:
        try:
            in_file = open(input_file_name, 'r')
        except IOError, e:
            print 'Opening %s failed: %s' % (input_file_name, e.what)
    print html_escape(in_file.read())

