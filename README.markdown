# nt-script.el: An Emacs library to use call-process or start-process for a script file on NTEmacs
## Overview
On NTEmacs, a script file cannot be specified for a argument call-process or start-process directry.
After loading this library, you can use call-process or start-process for a script file as follows.
<pre><code>(call-process "scriptfile")
</pre></code>
This library created based on mew-win32.el.
## How to use
Add your .emacs only a following line.
<pre><code>(require 'nt-script)
</pre></code>