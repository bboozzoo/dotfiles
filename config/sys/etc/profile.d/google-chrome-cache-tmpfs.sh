RUN_USER_PATH="/run/user/$USER"

if [[ -e $RUN_USER_PATH ]]; then
	mkdir -p /run/user/$USER/google-chrome-cache 2>/dev/null
fi


