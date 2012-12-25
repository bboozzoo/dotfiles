RUN_USER_PATH="/run/user"
user_id=$(id -u)

if [[ -e $RUN_USER_PATH ]]; then
	mkdir -p /run/user/$user_id/firefox-cache 2>/dev/null
fi

