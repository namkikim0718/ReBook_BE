package com.be.rebook.chat.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ChatMessage {
    private String sender;
    private String content;
    private String roomId; // 특정 채팅방을 구분하는 ID

    @Override
    public String toString() {
        return "ChatMessage{" +
                "sender='" + sender + '\'' +
                ", content='" + content + '\'' +
                ", roomId='" + roomId + '\'' +
                '}';
    }
}
