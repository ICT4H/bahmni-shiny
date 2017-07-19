# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.define "client" do |client|
    client.vm.box = "bento/centos-6.9"
    client.vm.network :private_network, ip: "192.168.33.10"
    client.vm.provider :virtualbox do |vb|
        vb.customize ["modifyvm", :id, "--memory", 2048, "--cpus", "2"]
    end
  end
end
