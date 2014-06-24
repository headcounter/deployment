{
  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=vda
    part swap --size=2000 --label=swap --fstype=swap --ondisk=vda
    part / --fstype=ext4 --label=root --grow --ondisk=vda
  '';
}
