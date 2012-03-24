{application,odl_manager,
	[
		{description,"Generic task manager"},

		{vsn,"0.1"},

		{modules, [
			odl_task,
			odl_queue,
			odl_manager
		]},

		{registered,[]},

		{applications, [
			kernel,
			stdlib,
			resource_discovery,
			fission
		]}

	]
}.
