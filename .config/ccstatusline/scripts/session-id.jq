#!/usr/bin/jq -rf
if .session_id then
  .session_id | split("-")[0]
else
  "N/A"
end
