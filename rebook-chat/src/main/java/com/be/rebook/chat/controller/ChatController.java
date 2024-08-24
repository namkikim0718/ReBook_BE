package com.be.rebook.chat.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.be.rebook.chat.dto.ChatMessageDTO;
import com.be.rebook.chat.dto.CreateChatRoomDto;
import com.be.rebook.chat.service.ChatService;
import com.be.rebook.chat.service.ChatSocketService;
import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@RestController
@RequiredArgsConstructor
@RequestMapping("/chat")
public class ChatController {

    private final ChatService chatService;

    private final ChatSocketService chatSocketService;

    @GetMapping("/rooms/{roomId}/messages")
    public ResponseEntity<BaseResponse<List<ChatMessageDTO>>> getChatRoomHistory( // TODO : AUTH 인증
            @NotNull(message = "roomId must not be null") @PathVariable Long roomId) {
        return ResponseEntity.ok().body(new BaseResponse<List<ChatMessageDTO>>(chatService.getChatRoomHistory(roomId)));
    }

    // @DeleteMapping("/rooms/{roomId}") // TODO : 채팅방 삭제 구현
    // public ResponseEntity<BaseResponse<Long>> deleteChatRoom( // TODO : AUTH 인증
    // @Auth MemberLoginInfo memberLoginInfo,
    // @NotNull(message = "roomId must not be null") @PathVariable Long roomId) {
    // return ResponseEntity.ok().body(new BaseResponse<Long>(deletedRoomId));
    // }

    @PostMapping("/rooms") // TODO : AUTH 인증
    public ResponseEntity<BaseResponse<Long>> createChatRoom(@RequestBody CreateChatRoomDto createChatRoomDto) {

        return ResponseEntity.ok().body(new BaseResponse<Long>(chatService.createChatRoom(createChatRoomDto)));
    }
}
