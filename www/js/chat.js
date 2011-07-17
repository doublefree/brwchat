/**
* chat.js
* @author takuya-watabe
*/

// chat message
var Chat = {};
Chat.Message = function() {
    var messageId = 0;

    var convertName = function(name) {
        var convertedName = name;
        switch (name) {
            case "system":
                convertedName = "システムメッセージ";
                break;
        }

        return convertedName;
    };

    var escape = function(val) {
        return $("<div/>").text(val).html();
    }

    var getMessageId = function() {
        return messageId;
    }

    var getMessage = function(handler) {
        $.get(handler, 
            {
                'messageId': getMessageId()
            },
            success = function(data) {
                var messages = JSON.parse(data);
                showMessage(messages);
            }
        );
    }

    var showMessage = function(messageList) {
        for(var i = 0; i < messageList.length; i++) {
            $("#chat_message").prepend(
                "<dt id=\"message" + messageList[i].log_id + "\">" +
                    escape(messageList[i].name) +
                "</dt>" +
                "<dd>" + 
                    "<div>" +
                    escape(messageList[i].message) +
                    "</div>" +
                "</dd>");
            if(messageId < messageList[i].log_id) {
                messageId = messageList[i].log_id;
            }
        }
    }

    return {
        'getMessageId' : getMessageId,
        'getMessage' : getMessage, 
        'showMessage' : showMessage
    }
};

var chat_message = Chat.Message();

// onload
$(function() {
    const chat_handler = 'chat_handler.yaws';
    $('#login').submit(function() {
        var login_name = $('#name').val();
        if (login_name.length == 0) {
            alert('please input your name');
            return false;
        }

        $.post(chat_handler, 
            {
                name: login_name
            },
            success = function(data) {
            alert(data);
                // hide container
                $("#login_container").hide();
            }
        );

        return false;
    });
    $('#get_message').submit(function() {
        chat_message.getMessage(chat_handler);
        return false;
    });
    $('#post_message').submit(function() {
        var message = $('#message').val();
        if (message.length == 0) {
            return false;
        }

        $.post(chat_handler,
            {
                'message': message
            },
            success = function(data) {
                $('#message').val('');
            }
        );

        return false;
    });
});

