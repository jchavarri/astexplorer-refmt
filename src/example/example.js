let code = "let x = 1";

let ast = window.parseReason(code);
window.document.querySelector("#app").innerHTML = ast;
