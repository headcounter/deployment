lib:

{
  enumDoc = attrs: ''
    <variablelist>
      ${lib.concatStrings (lib.flip lib.mapAttrsToList attrs (option: doc: ''
        <varlistentry>
          <term><option>${option}</option></term>
          <listitem><para>${doc}</para></listitem>
        </varlistentry>
      ''))}
    </variablelist>
  '';
}
