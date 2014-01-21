#!/usr/bin/python

def get_password(user_name):
    import keyring
    passwd = keyring.get_password('mail', user_name)

    return passwd


def get_or_set_password(user_name):
    passwd = get_password(user_name)

    if not passwd:
        passwd = set_password(user_name)

    return passwd

def set_password(user_name):
    passwd = None

    import getpass
    pw = getpass.getpass('password for %s: ' % (user_name))
    if pw:
        passwd = pw

    if passwd:
        import keyring
        keyring.set_password('mail', user_name, passwd)

    return passwd


def main():
    """
    """
    import sys

    if len(sys.argv) < 3:
        raise SystemExit(5)

    cmd = sys.argv[1]
    user = sys.argv[2]
    if cmd == 'set':
        set_password(user)
    elif cmd == 'get':
        passwd = get_password(user)
        if not passwd:
            print 'not set'
            raise SystemExit(2)
        else:
            print passwd
    else:
        print 'uknown command'
        raise SystemExit(3)

if __name__ == '__main__':
    main()
