/**
* chat.js
* @author takuya-watabe
*/

// onload
$(function() {
    $('#login').submit(function() {
        var login_name = $('#name').val();
        if (login_name.length == 0) {
            alert('please input your name');
            return false;
        }

        $.post('chat_handler.yaws', 
            {
                name: login_name
            },
            success = function(data) {
            alert(data);
                // hide container
                //$("#login_container").hide();
            }
        );

        return false;
    });
});
