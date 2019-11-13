// Just goofing around here with the logo.

var logos = [
  'perplex<span id="bind"> &lt&gt </span>dev',
  'perplex<span id="bind"> &lt&#36&gt </span>dev',
  'perplexed<span id="bind">/</span>dev<span id="bind">/</span>',
  'perplex<span id="bind"> =&lt&lt </span>dev',
  'perplexed<span id="bind">.</span>dev'
];

function randLogo() {
  var randIndex = Math.floor(Math.random() * logos.length);
  document.getElementById("logo").innerHTML = logos[randIndex];
  document.getElementById("bind").style.color = getRandColor()
}

function getRandColor() {
  var letters = '0123456789ABCDEF';
  var color = '#';
  for (var i = 0; i < 6; i++) {
    color += letters[Math.floor(Math.random() * 16)];
  }
  return color;
}
