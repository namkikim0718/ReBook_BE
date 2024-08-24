package com.be.rebook.chat.controller;

import java.util.List;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.be.rebook.chat.dto.ChatMessageDTO;
import com.be.rebook.chat.dto.ChatRoomDto;
import com.be.rebook.chat.dto.CreateChatRoomDto;
import com.be.rebook.chat.service.ChatService;
import com.be.rebook.chat.service.ChatSocketService;
import com.be.rebook.common.argumentresolver.auth.Auth;
import com.be.rebook.common.argumentresolver.auth.MemberLoginInfo;
import com.be.rebook.common.config.BaseResponse;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;
import com.be.rebook.common.restclients.RestClientFactory;

import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@RestController
@RequiredArgsConstructor
@RequestMapping("/chat")
public class ChatController {

    private final ChatService chatService;

    private final RestClientFactory restClientFactory;

    @GetMapping("/rooms/{roomId}/messages")
    public ResponseEntity<BaseResponse<List<ChatMessageDTO>>> getChatRoomHistory( // TODO : AUTH 인증
            @NotNull(message = "roomId must not be null") @PathVariable Long roomId) {
        return ResponseEntity.ok().body(new BaseResponse<List<ChatMessageDTO>>(chatService.getChatRoomHistory(roomId)));
    }

    @GetMapping("/me/rooms")
    public ResponseEntity<BaseResponse<List<ChatRoomDto>>> getMethodName(@Auth MemberLoginInfo memberLoginInfo) {
        if (memberLoginInfo == null) { // TODO : AUTH 비동기 문제 해결되면 빼도됨
            throw new BaseException(ErrorCode.UNAUTHORIZED); // TODO : 적절한 예외처리 변경
        }
        return ResponseEntity.ok()
                .body(new BaseResponse<List<ChatRoomDto>>(
                        chatService.findChatRoomsByUsername(memberLoginInfo.getUsername())));
    }

    // @DeleteMapping("/rooms/{roomId}") // TODO : 채팅방 삭제 구현 (listener도 삭제해야함)
    // public ResponseEntity<BaseResponse<Long>> deleteChatRoom( // TODO : AUTH 인증
    // @Auth MemberLoginInfo memberLoginInfo,
    // @NotNull(message = "roomId must not be null") @PathVariable Long roomId) {
    // return ResponseEntity.ok().body(new BaseResponse<Long>(deletedRoomId));
    // }

    // TODO : 서버간 통신 인터페이스 작성 완료시 상품의 판매자와 요청이 일치하는지 검사해야함
    @PostMapping("/rooms")
    public ResponseEntity<BaseResponse<Long>> createChatRoom(@Auth MemberLoginInfo memberLoginInfo,
            @RequestBody CreateChatRoomDto createChatRoomDto) {
        if (memberLoginInfo == null) { // TODO : AUTH 비동기 문제 해결되면 빼도됨
            throw new BaseException(ErrorCode.UNAUTHORIZED); // TODO : 적절한 예외처리 변경
        }

        return ResponseEntity.ok().body(new BaseResponse<Long>(chatService.createChatRoom(createChatRoomDto)));
    }
}
