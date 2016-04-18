#!/usr/bin/python
#
# The MIT License (MIT)
#
# Copyright (c) 2016 Maciej Borzecki
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from __future__ import print_function
import argparse
import logging
import threading

# workaround warning in pykeyring when inporting GnomeKeyring
import gi
gi.require_version('GnomeKeyring', '1.0')

from keyring.backends.SecretService import Keyring


LOCK = threading.RLock()


def get_password(user_name):

    passwd = None
    try:
        with LOCK:
            store = Keyring()
            passwd = store.get_password('mail', user_name)
    except Exception:
        logging.exception('failed getting password')

    return passwd


def get_or_set_password(user_name):

    with LOCK:
        try:
            passwd = get_password(user_name)

            if not passwd:
                passwd = set_password(user_name)
        except Exception as ex:
            logging.exception('error getting or setting password')

    return passwd


def set_password(user_name):
    import getpass
    import keyring

    passwd = None

    pw = getpass.getpass('password for %s: ' % (user_name))
    if pw:
        passwd = pw

    if passwd:
        try:
            with LOCK:
                store = Keyring()
                store.set_password('mail', user_name, passwd)
        except Exception:
            logging.exception('failed setting password')


    return passwd

def parse_arguments():
    parser = argparse.ArgumentParser(description='Password helper')
    parser.add_argument('-m', '--mutt',
                        dest='mutt_name',
                        type=str, default='',
                        help='output a mutt password stanza for given account')

    parser.add_argument('cmd', help='command (get|set)')
    parser.add_argument('account', help='account')

    return parser.parse_args()

def main():
    """
    """

    logging.basicConfig(level=logging.DEBUG)

    opts = parse_arguments()

    if opts.cmd == 'set':
        set_password(opts.account)
    elif opts.cmd == 'get':
        passwd = get_password(opts.account)
        if not passwd:
            print('not set')
            raise SystemExit(2)
        else:
            if opts.mutt_name:
                print('set {:s}={:s}'.format(opts.mutt_name, passwd))
            else:
                print(passwd)
    else:
        logging.error('unknown command')
        raise SystemExit(3)

if __name__ == '__main__':
    main()
