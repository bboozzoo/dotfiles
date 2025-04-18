terraform {
  required_providers {
    docker = {
      source  = "kreuzwerker/docker"
      version = "3.0.2"
    }
  }
}

provider "docker" {}

resource "docker_image" "homeassistant" {
  name         = "ghcr.io/home-assistant/home-assistant:stable"
  keep_locally = true
}

resource "docker_container" "homeassistant" {
  image        = docker_image.homeassistant.image_id
  name         = "homeassistant"
  network_mode = "host"
  log_driver   = "journald"
  restart      = "unless-stopped"
  privileged   = true

  volumes {
    container_path = "/config"
    host_path      = var.local_config_path
  }

  volumes {
    container_path = "/etc/localtime"
    host_path      = "/etc/localtime"
    read_only      = true
  }

  volumes {
    container_path = "/run/dbus"
    host_path      = "/run/dbus"
    read_only      = true
  }
}
