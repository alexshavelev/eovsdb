-define(OVSREC_DB_NAME, "Open_vSwitch").

%% Bridge Definitions
-define(OVSREC_TABLE_BRIDGE, "Bridge").
-define(OVSREC_BRIDGE_COL_CONTROLLER, "controller").
-define(OVSREC_BRIDGE_COL_DATAPATH_ID, "datapath_id").
-define(OVSREC_BRIDGE_COL_DATAPATH_TYPE, "datapath_type").
-define(OVSREC_BRIDGE_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_BRIDGE_COL_FAIL_MODE, "fail_mode").
-define(OVSREC_BRIDGE_COL_FLOOD_VLANS, "flood_vlans").
-define(OVSREC_BRIDGE_COL_FLOW_TABLES, "flow_tables").
-define(OVSREC_BRIDGE_COL_MIRRORS, "mirrors").
-define(OVSREC_BRIDGE_COL_NAME, "name").
-define(OVSREC_BRIDGE_COL_NETFLOW, "netflow").
-define(OVSREC_BRIDGE_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_BRIDGE_COL_PORTS, "ports").
-define(OVSREC_BRIDGE_COL_SFLOW, "sflow").
-define(OVSREC_BRIDGE_COL_STATUS, "status").
-define(OVSREC_BRIDGE_COL_STP_ENABLE, "stp_enable").

%% Controller Definitions
-define(OVSREC_TABLE_CONTROLLER, "Controller").
-define(OVSREC_CONTROLLER_COL_CONNECTION_MODE, "connection_mode").
-define(OVSREC_CONTROLLER_COL_CONTROLLER_BURST_LIMIT, "controller_burst_limit").
-define(OVSREC_CONTROLLER_COL_CONTROLLER_RATE_LIMIT, "controller_rate_limit").
-define(OVSREC_CONTROLLER_COL_ENABLE_ASYNC_MESSAGES, "enable_async_messages").
-define(OVSREC_CONTROLLER_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_CONTROLLER_COL_INACTIVITY_PROBE, "inactivity_probe").
-define(OVSREC_CONTROLLER_COL_IS_CONNECTED, "is_connected").
-define(OVSREC_CONTROLLER_COL_LOCAL_GATEWAY, "local_gateway").
-define(OVSREC_CONTROLLER_COL_LOCAL_IP, "local_ip").
-define(OVSREC_CONTROLLER_COL_LOCAL_NETMASK, "local_netmask").
-define(OVSREC_CONTROLLER_COL_MAX_BACKOFF, "max_backoff").
-define(OVSREC_CONTROLLER_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_CONTROLLER_COL_ROLE, "role").
-define(OVSREC_CONTROLLER_COL_STATUS, "status").
-define(OVSREC_CONTROLLER_COL_TARGET, "target").

%% Flow Table Definitions
-define(OVSREC_TABLE_FLOW_TABLE, "Flow_Table").
-define(OVSREC_FLOW_TABLE_COL_FLOW_LIMIT, "flow_limit").
-define(OVSREC_FLOW_TABLE_COL_GROUPS, "groups").
-define(OVSREC_FLOW_TABLE_COL_NAME, "name").
-define(OVSREC_FLOW_TABLE_COL_OVERFLOW_POLICY, "overflow_policy").

%% Interface Definitions
-define(OVSREC_TABLE_INTERFACE, "Interface").
-define(OVSREC_INTERFACE_COL_ADMIN_STATE, "admin_state").
-define(OVSREC_INTERFACE_COL_CFM_FAULT, "cfm_fault").
-define(OVSREC_INTERFACE_COL_CFM_FAULT_STATUS, "cfm_fault_status").
-define(OVSREC_INTERFACE_COL_CFM_HEALTH, "cfm_health").
-define(OVSREC_INTERFACE_COL_CFM_MPID, "cfm_mpid").
-define(OVSREC_INTERFACE_COL_CFM_REMOTE_MPIDS, "cfm_remote_mpids").
-define(OVSREC_INTERFACE_COL_CFM_REMOTE_OPSTATE, "cfm_remote_opstate").
-define(OVSREC_INTERFACE_COL_DUPLEX, "duplex").
-define(OVSREC_INTERFACE_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_INTERFACE_COL_INGRESS_POLICING_BURST, "ingress_policing_burst").
-define(OVSREC_INTERFACE_COL_INGRESS_POLICING_RATE, "ingress_policing_rate").
-define(OVSREC_INTERFACE_COL_LACP_CURRENT, "lacp_current").
-define(OVSREC_INTERFACE_COL_LINK_RESETS, "link_resets").
-define(OVSREC_INTERFACE_COL_LINK_SPEED, "link_speed").
-define(OVSREC_INTERFACE_COL_LINK_STATE, "link_state").
-define(OVSREC_INTERFACE_COL_MAC, "mac").
-define(OVSREC_INTERFACE_COL_MTU, "mtu").
-define(OVSREC_INTERFACE_COL_NAME, "name").
-define(OVSREC_INTERFACE_COL_OFPORT, "ofport").
-define(OVSREC_INTERFACE_COL_OPTIONS, "options").
-define(OVSREC_INTERFACE_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_INTERFACE_COL_STATISTICS, "statistics").
-define(OVSREC_INTERFACE_COL_STATUS, "status").
-define(OVSREC_INTERFACE_COL_TYPE, "type").

%% Manager Definitions
-define(OVSREC_TABLE_MANAGER, "Manager").
-define(OVSREC_MANAGER_COL_CONNECTION_MODE, "connection_mode").
-define(OVSREC_MANAGER_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_MANAGER_COL_INACTIVITY_PROBE, "inactivity_probe").
-define(OVSREC_MANAGER_COL_IS_CONNECTED, "is_connected").
-define(OVSREC_MANAGER_COL_MAX_BACKOFF, "max_backoff").
-define(OVSREC_MANAGER_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_MANAGER_COL_STATUS, "status").
-define(OVSREC_MANAGER_COL_TARGET, "target").

%% Mirror Definitions
-define(OVSREC_TABLE_MIRROR, "Mirror").
-define(OVSREC_MIRROR_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_MIRROR_COL_NAME, "name").
-define(OVSREC_MIRROR_COL_OUTPUT_PORT, "output_port").
-define(OVSREC_MIRROR_COL_OUTPUT_VLAN, "output_vlan").
-define(OVSREC_MIRROR_COL_SELECT_ALL, "select_all").
-define(OVSREC_MIRROR_COL_SELECT_DST_PORT, "select_dst_port").
-define(OVSREC_MIRROR_COL_SELECT_SRC_PORT, "select_src_port").
-define(OVSREC_MIRROR_COL_SELECT_VLAN, "select_vlan").
-define(OVSREC_MIRROR_COL_STATISTICS, "statistics").

%% NetFlow Definitions
-define(OVSREC_TABLE_NETFLOW, "NetFlow").
-define(OVSREC_NETFLOW_COL_ACTIVE_TIMEOUT, "active_timeout").
-define(OVSREC_NETFLOW_COL_ADD_ID_TO_INTERFACE, "add_id_to_interface").
-define(OVSREC_NETFLOW_COL_ENGINE_ID, "engine_id").
-define(OVSREC_NETFLOW_COL_ENGINE_TYPE, "engine_type").
-define(OVSREC_NETFLOW_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_NETFLOW_COL_TARGETS, "targets").

%% vSwitch Definitions
-define(OVSREC_TABLE_OPEN_VSWITCH, "Open_vSwitch").
-define(OVSREC_OPEN_VSWITCH_COL_BRIDGES, "bridges").
-define(OVSREC_OPEN_VSWITCH_COL_CUR_CFG, "cur_cfg").
-define(OVSREC_OPEN_VSWITCH_COL_DB_VERSION, "db_version").
-define(OVSREC_OPEN_VSWITCH_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_OPEN_VSWITCH_COL_MANAGER_OPTIONS, "manager_options").
-define(OVSREC_OPEN_VSWITCH_COL_NEXT_CFG, "next_cfg").
-define(OVSREC_OPEN_VSWITCH_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_OPEN_VSWITCH_COL_OVS_VERSION, "ovs_version").
-define(OVSREC_OPEN_VSWITCH_COL_SSL, "ssl").
-define(OVSREC_OPEN_VSWITCH_COL_STATISTICS, "statistics").
-define(OVSREC_OPEN_VSWITCH_COL_SYSTEM_TYPE, "system_type").
-define(OVSREC_OPEN_VSWITCH_COL_SYSTEM_VERSION, "system_version").

%% Port Definitions
-define(OVSREC_TABLE_PORT, "Port").
-define(OVSREC_PORT_COL_BOND_DOWNDELAY, "bond_downdelay").
-define(OVSREC_PORT_COL_BOND_FAKE_IFACE, "bond_fake_iface").
-define(OVSREC_PORT_COL_BOND_MODE, "bond_mode").
-define(OVSREC_PORT_COL_BOND_UPDELAY, "bond_updelay").
-define(OVSREC_PORT_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_PORT_COL_FAKE_BRIDGE, "fake_bridge").
-define(OVSREC_PORT_COL_INTERFACES, "interfaces").
-define(OVSREC_PORT_COL_LACP, "lacp").
-define(OVSREC_PORT_COL_MAC, "mac").
-define(OVSREC_PORT_COL_NAME, "name").
-define(OVSREC_PORT_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_PORT_COL_QOS, "qos").
-define(OVSREC_PORT_COL_STATISTICS, "statistics").
-define(OVSREC_PORT_COL_STATUS, "status").
-define(OVSREC_PORT_COL_TAG, "tag").
-define(OVSREC_PORT_COL_TRUNKS, "trunks").
-define(OVSREC_PORT_COL_VLAN_MODE, "vlan_mode").

%% QoS Definitions
-define(OVSREC_TABLE_QOS, "QoS").
-define(OVSREC_QOS_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_QOS_COL_OTHER_CONFIG, "other_config").
-define(OVSREC_QOS_COL_QUEUES, "queues").
-define(OVSREC_QOS_COL_TYPE, "type").

%% Queue Definitions
-define(OVSREC_TABLE_QUEUE, "Queue").
-define(OVSREC_QUEUE_COL_DSCP, "dscp").
-define(OVSREC_QUEUE_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_QUEUE_COL_OTHER_CONFIG, "other_config").

%% SSL Definitions
-define(OVSREC_TABLE_SSL, "SSL").
-define(OVSREC_SSL_COL_BOOTSTRAP_CA_CERT, "bootstrap_ca_cert").
-define(OVSREC_SSL_COL_CA_CERT, "ca_cert").
-define(OVSREC_SSL_COL_CERTIFICATE, "certificate").
-define(OVSREC_SSL_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_SSL_COL_PRIVATE_KEY, "private_key").

%% sFlow Definitions
-define(OVSREC_TABLE_SFLOW, "sFlow").
-define(OVSREC_SFLOW_COL_AGENT, "agent").
-define(OVSREC_SFLOW_COL_EXTERNAL_IDS, "external_ids").
-define(OVSREC_SFLOW_COL_HEADER, "header").
-define(OVSREC_SFLOW_COL_POLLING, "polling").
-define(OVSREC_SFLOW_COL_SAMPLING, "sampling").
-define(OVSREC_SFLOW_COL_TARGETS, "targets").
