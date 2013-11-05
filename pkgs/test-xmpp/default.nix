{ buildPythonPackage, xmpppy }:

buildPythonPackage {
  name = "test-xmpp";

  propagatedBuildInputs = [ xmpppy ];

  unpackPhase = ''
    cat > setup.py <<EOF
    from distutils.core import setup
    setup(name='test-xmpp', scripts=['test-xmpp'])
    EOF
    cp ${./test-xmpp.py} test-xmpp
  '';

  doCheck = false;
}
