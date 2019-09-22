  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

terraform fmt

  $ run_restyler terraform outputs.tf variables.tf
  outputs.tf
  variables.tf
     value = "${azurerm_container_group.aci-example.ip_address}"
   }
   
  -output "fqdn"
  -{
  -    value = "${azurerm_container_group.aci-example.fqdn}"
  +output "fqdn" {
  +  value = "${azurerm_container_group.aci-example.fqdn}"
   }
   variable "policy_definition_name" {
  -    description  = "Policy definition name must only contain lowercase letters, digits or dashes, cannot use dash as the first two or last one characters, cannot contain consecutive dashes, and is limited between 2 and 60 characters in length."
  -    default    = "demoPolicy"
  +  description = "Policy definition name must only contain lowercase letters, digits or dashes, cannot use dash as the first two or last one characters, cannot contain consecutive dashes, and is limited between 2 and 60 characters in length."
  +  default     = "demoPolicy"
   }
  -
