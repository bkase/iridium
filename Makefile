IDRIS := idris

quartz:
	${IDRIS} --build iridium-quartz.ipkg
	${IDRIS} -i src -p effects -p contrib -o iridium Quartz
