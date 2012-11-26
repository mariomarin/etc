_dotfile_root="$HOME/etc"; _dotfile_trash="$HOME/tmp/_df_trash"; _managed_files=()
_dotfile_list="$(find "$_dotfile_root" -name ".*" ! -name ".git" -print)"

_special_files="
$_dotfile_root/irc/irssi .irssi/config
$_dotfile_root/xorg/dunstrc .config/dunstrc
$_dotfile_root/web/googlecl-config .config/googlecl/config
$_dotfile_root/web/newsbeuter-config .newsbeuter/config
.xmonad/taffybar .config/taffybar
.profile .zshrc
.profile .zprofile"

_blacklist_files="
.bashrc
.bash_logout
.bash_profile"

ORIG_IFS=$IFS; LINE_IFS=$(echo -en "\n\b")
cd $HOME; rm -rf "$_dotfile_trash"; mkdir -p "$_dotfile_trash"

_Link () { _source="$1"; _target="$2" ln -fs "$_source" "$_target" && _managed_files=(${_managed_files[@]} "$_target") || echo "FAILED TO LINK $_source"; }

_Link_Normal () {
_source="$1"; _target="$(basename "$_source")";
[ -e "$_target" -a ! -L "$_target" ] && mv "$_target" "$_dotfile_trash";
[ -L "$_target" ] && rm "$_target"; # to avoid doubling up directories & ensure correct link
_Link "$_source" "$_target"; }

_Link_Special () {
_source="$1"; _target="$2"; _target_parent="$(dirname "$_target")";
echo "_source==$_source"
echo "_target==$_target"
echo "_target_parent==$_target_parent"
[ ! -d "$_target_parent" ] && mkdir -p "$_target_parent";
[ -e "$_target" -a ! -L "$_target" ] && mv "$_target" "$_dotfile_trash";
[ -L "$_target" ] && rm "$_target"; # to avoid doubling up directories & ensure correct link
_Link "$_source" "$_target"; }

IFS=$LINE_IFS; for _normal_file in $_dotfile_list; do _Link_Normal "$_normal_file"; done; IFS=$ORIG_IFS
IFS=$LINE_IFS; for _special_file in $_special_files; do IFS=$ORIG_IFS; _Link_Special $_special_file; IFS=$LINE_IFS; done; IFS=$ORIG_IFS
IFS=$LINE_IFS; for _blacklist_file in $_blacklist_files; do IFS=$ORIG_IFS; rm -rf $_blacklist_file &>/dev/null; IFS=$LINE_IFS; done; IFS=$ORIG_IFS
_garbage="$(ls -A1)"; for _file in "${_managed_files[@]}"; do _garbage="$(echo "$_garbage" | grep -v "$_file")"; done

chmod 600 $HOME/.msmtprc

echo -e "\n\nFollowing dotfiles are managed:\n---------------------------------\n${_managed_files[@]}"
echo -e "\nFollowing dotfiles are unmanaged:\n---------------------------------"; echo $(echo "$_garbage" | egrep "^\.")
echo

