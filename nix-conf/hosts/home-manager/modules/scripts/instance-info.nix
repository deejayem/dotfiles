{ pkgs }:

pkgs.writeShellScriptBin "instance-info" ''
  set -euo pipefail

  if [ $# -eq 0 ]; then
      echo "Usage: instance-info <instance-id> [<instance-id> ...]" >&2
      exit 1
  fi

  instance_ids=("$@")

  # Get EC2 instance details
  ec2_info=$(${pkgs.awscli2}/bin/aws ec2 describe-instances \
      --instance-ids "''${instance_ids[@]}" \
      | ${pkgs.json-table}/bin/jt Reservations [ ] Instances [ ] \
          [ InstanceId % ] \
          [ LaunchTime % ] \
          [ Placement AvailabilityZone % ] \
          [ ImageId % ] \
          [ LaunchTemplate LaunchTemplateName % ] \
          [ LaunchTemplate Version % ] \
          [ State Name % ] \
          [ PrivateIpAddress % ] \
      | sort)

  # Get ASG information
  asg_info=$(${pkgs.awscli2}/bin/aws autoscaling describe-auto-scaling-instances \
      --instance-ids "''${instance_ids[@]}" \
      | ${pkgs.json-table}/bin/jt AutoScalingInstances [ ] \
          [ InstanceId % ] \
          [ AutoScalingGroupName % ] \
          [ LifecycleState % ] \
          [ HealthStatus % ] \
          [ ProtectedFromScaleIn % ] \
      | sort)

  # Join the two on InstanceId
  join -t $'\t' -1 1 -2 1 \
      <(echo "$ec2_info") \
      <(echo "$asg_info")
''
