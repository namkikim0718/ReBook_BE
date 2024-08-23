package com.be.rebook.chat.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class ChatMessage {
    private Long senderId;
    private MessageType type;
    private String message;
    private Long roomId; // 특정 채팅방을 구분하는 ID

    public enum MessageType {
        JOIN, TALK, ENTER
    }

    @Override
    public String toString() {
        return "ChatMessage{" +
                "senderId='" + senderId + '\'' +
                ", message='" + message + '\'' +
                ", roomId='" + roomId + '\'' +
                '}';
    }
}
