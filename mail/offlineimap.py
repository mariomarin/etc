#prioritized = ['inbox', 'flagged', 'important', 'archive', 'sent', 'spam', 'drafts']
prioritized = ['INBOX', '[Gmail]/Starred', '[Gmail]/Important']

def mycmp(x, y):
  for prefix in prioritized:
    xsw = x.startswith(prefix)
    ysw = y.startswith(prefix)
    if xsw and ysw:
      return cmp(x, y)
    elif xsw:
      return -1
    elif ysw:
      return +1
  return cmp(x, y)

def test_mycmp():
  import os, os.path
  folders=os.listdir(os.path.expanduser('~/var/mail/accounts/es'))
  folders.sort(mycmp)
  print folders

