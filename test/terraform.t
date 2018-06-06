  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

terraform fmt

  $ run_restyler_cmd terraform terraform-fmt-multi -- outputs.tf variables.tf
  outputs.tf
  variables.tf
  diff --git i/outputs.tf w/outputs.tf
  index d375be5..8e57a0d 100644
  --- i/outputs.tf
  +++ w/outputs.tf
  @@ -2,7 +2,6 @@ output "ip_address" {
     value = "${azurerm_container_group.aci-example.ip_address}"
   }
   
  -output "fqdn"
  -{
  -    value = "${azurerm_container_group.aci-example.fqdn}"
  +output "fqdn" {
  +  value = "${azurerm_container_group.aci-example.fqdn}"
   }
  diff --git i/variables.tf w/variables.tf
  index 7fd5f00..b5ef381 100644
  --- i/variables.tf
  +++ w/variables.tf
  @@ -1,5 +1,4 @@
   variable "policy_definition_name" {
  -    description  = "Policy definition name must only contain lowercase letters, digits or dashes, cannot use dash as the first two or last one characters, cannot contain consecutive dashes, and is limited between 2 and 60 characters in length."
  -    default    = "demoPolicy"
  +  description = "Policy definition name must only contain lowercase letters, digits or dashes, cannot use dash as the first two or last one characters, cannot contain consecutive dashes, and is limited between 2 and 60 characters in length."
  +  default     = "demoPolicy"
   }
  -
