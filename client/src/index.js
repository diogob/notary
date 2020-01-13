import JWK from "node-jose";
import scrypt from "scrypt-js";
import {Buffer} from "buffer/";

function get(id) { return document.getElementById(id); }
function normalized(field) {
    var value = get('pbkdf-' + field).value;
    var forms = document.getElementById('form-' + field).getElementsByClassName('selected');
    if (forms.length !== 1) { throw new Error('missing form'); }
    var form = forms[0].innerHTML;

    if (form.indexOf('NFKC') >= 0) {
        return Buffer.from(value.normalize('NFKC'), 'utf8');
    } else if (form.indexOf('NFKD') >= 0) {
        return Buffer.from(value.normalize('NFKD'), 'utf8');
    } else if (form.indexOf('hex') >= 0) {
        if (!value.match(/^([0-9A-F][0-9A-F])*$/i)) {
            throw new Error(field + ': invalid hex string');
        }
        return new Buffer.from(value, 'hex');
    }

    throw new Error('Unknown ');
}

var firstLine = true;
function clearConsole() {
    firstLine = true;
    get('result').innerHTML = '';
}

function printConsole(message) {
    if (!firstLine) { message = '<br /><br />' + message; }
    firstLine = false;
    get('result').innerHTML += message;
}

(function() {
    var forms = document.getElementsByClassName('form');
    for (var i = 0; i < forms.length; i++) {
        forms[i].onclick = (function(form) {
            return function() {
                console.log(form);
                var selected = form.parentNode.getElementsByClassName('selected')[0];
                selected.classList.remove('selected');
                form.classList.add('selected');
            };
        })(forms[i]);
    }
})();

var submit = get('pbkdf-submit');

var done = null;
submit.onclick = function() {
    if (done === null) {
        clearConsole();

        done = false;

        try {
            var password = normalized('password'); //get('pbkdf-password').value;
            var salt = normalized('salt');// get('pbkdf-salt').value;
            var N = 1 << parseInt(get('pbkdf-Nlog2').value);
            var r = get('pbkdf-r').value;
            var p = get('pbkdf-p').value;
            var dkLen = get('pbkdf-dkLen').value;
            console.log(password, salt, N, r, p)

        } catch (error) {
            printConsole(error.message);

            done = null;
            return;
        }

        submit.classList.add('cancel');
        submit.value = "Cancel scrypt";

        printConsole('Started: N=' + N + ', r=' + r + ' p=' + p +
            ', password=0x' + password.toString('hex') + ', salt=0x' + salt.toString('hex'));

        var t0 = (new Date()).getTime();

        var keyP = scrypt.scrypt(password, salt, parseInt(N), parseInt(r), parseInt(p), parseInt(dkLen), function(progress) {
            get('progressBar').style.width = parseInt(100 * progress) + '%';
            get('progressAmount').innerHTML = parseInt(100 * progress) + '%';
        });
        keyP.then(function(key) {
            var keyBuffer = Buffer.from(key);
            printConsole("Generated: " + keyBuffer.toString('hex'));
            // var myrng = new Math.seedrandom(keyBuffer.toString('hex'));
            // printConsole("Random number: " + myrng());
        });

    } else if (done === false) {
        done = true;
    }
};
