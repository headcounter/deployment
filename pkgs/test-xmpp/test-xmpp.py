import argparse
import xmpp

def do_register(args):
    jid = xmpp.protocol.JID(args.jid)

    client = xmpp.Client(jid.getDomain())
    client.connect()

    credentials = {
        'username': jid.getNode(),
        'password': args.passwd,
    }

    xmpp.features.getRegInfo(client, jid.getDomain(), credentials, sync=True)
    return xmpp.features.register(client, jid.getDomain(), credentials)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('jid', type=str)
    parser.add_argument('--passwd', type=str, default="abcde")

    subparsers = parser.add_subparsers()
    parser_register = subparsers.add_parser('register')
    parser_register.set_defaults(func=do_register)

    args = parser.parse_args()
    if not args.func(args):
        parser.exit(1)
