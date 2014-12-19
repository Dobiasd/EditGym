#EditGym

###Text editing training

---

A lot of daily tasks require you to edit text. And no matter if you use a word processor like [LibreOffice Writer](https://www.libreoffice.org/discover/writer) or [Word](http://en.wikipedia.org/wiki/Microsoft_Word), an editor like [Notepad(++)](http://notepad-plus-plus.org), an [IDE](http://en.wikipedia.org/wiki/Integrated_development_environment) like [Eclipse](https://eclipse.org) etc. or just write an email or a post on [facebook](http://www.facebook.com) or [reddit](http://www.reddit.com) in your browser, the most basic default keyboard shortcuts are the same. And knowing them can [save you](http://acrobolix.com/keyboarding-changed-my-life/) [precious time](http://lifehacker.com/5970089/back-to-the-basics-learn-to-use-keyboard-shortcuts-like-a-ninja).

But real efficiency is not reached by [just memorizing all shortcuts](https://www.shortcutfoo.com). It is also important (and fun!) to be able to effortlessly choose the tactics with the smallest amount of shortcuts to reach the desired goal.

EditGym helps you to acquire this set of skills by letting you practice and compete on standardized text snippets.

---

todo:
finished: Coach: "Your score: 42.1s, 23 key moves. You can go on to the next exercise (ctrl+n), try to improve (ctrl+r) or register your result to the highscore list (ctrl+h)."
stars: display when finished, in exercise header and in exercise list
save stars with web storage http://stackoverflow.com/

beim highscore-eintrag auswaehlen lassen ob time, keys oder beides
highscore position regarding last 100 users of this exercise anzeigen
highscore-dialog am Ende automatisch über das game einblenden
highscore sorting: if criterion 1 is equal then crit 2, wenn auch egal dann timestamp, wenn das equal dann name. Kombination aus allem unique in der DB.
no negative values in highscore (keys and time)
highscore spam von einer ip verhindern
email an mich schicken wenn die highscore-liste eine zweierpotenz erreicht
ads
custom exercises: as url or in online raw text (e.g. codepad). url only up to 2000 total. (http://stackoverflow.com/questions/417142/what-is-the-maximum-length-of-a-url-in-different-browsers)
custom exercises: remove unallowed chars
replay in help (gif?)
zeit verstellen verhindern, timestamps aller keymoves monoton steigend
newsletter abbo (keep me informed), email an mich bei zweierpotenz
https for sending?
FB-page einrichten, like button oben als link dahin
Highscore: Histogram mit vertical line wo man steht
best of today anzeigen
questions/16427636/check-if-localstorage-is-available
highscrore string: use xor encoding in elm, then port to js, then to server, then xor reverse. have everything twice in string an check this redundancy for equality.

when done, post here:
http://acrobolix.com/keyboarding-changed-my-life/ "Hi, I made a website for practicing to rearrange text with keyboard shortcuts. www.editgym.com"
http://www.reddit.com/r/productivity/ "Become lightning fast at rearraning text with EditGym.com"
http://www.reddit.com/r/webdev EditGym.com - A website for practicing to rearrange text with keyboard shortcuts (hobby project)

optimize performance

quatsch:
instant replay
compete live (with chat)